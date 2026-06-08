{
description = "hbs2";

inputs = {

    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    # haskell-flake-utils.url = "github:ivanovs-4/haskell-flake-utils";
    flake-utils.url = "github:numtide/flake-utils";
    haskell-flake-utils = { # we don't use haskell-flake-utils directly, but we override input evrywhere
        url = "github:ivanovs-4/haskell-flake-utils/master";
        inputs.flake-utils.follows = "flake-utils";
    };
    hspup.url = "github:voidlizard/hspup";
    hspup.inputs.nixpkgs.follows = "nixpkgs";
    hspup.inputs.haskell-flake-utils.follows = "haskell-flake-utils";

};

outputs = { self, nixpkgs, flake-utils, ... }@inputs:
  let
    packageNames =
      topLevelPackages ++ keymanPackages;

    keymanPackages =
      [
      "hbs2-keyman"
      "hbs2-keyman-direct-lib"
      ];

    topLevelPackages =
      [
      "hbs2-peer"
      "hbs2-core"
      "hbs2-storage-simple"
      "hbs2-storage-ncq"
      "hbs2-git3"
      "hbs2-cli"
      "hbs2-sync"
      "hbs2-log-structured"
      ];

    miscellaneous =
      [
      "db-pipe"
      "fuzzy-parse"
      "suckless-conf"
      ];

    jailbreakUnbreak = pkgs: pkg:
        pkgs.haskell.lib.doJailbreak (pkg.overrideAttrs (_: { meta = { }; }));

    # gitHbs2Script = pkgs.stdenv.mkDerivation {
    #   pname = "git-hbs2";
    #   version = "1.0";
    #   src = ./hbs2-git3/bf6;
    #   installPhase = ''
    #     mkdir -p $out/bin
    #     install -m755 git-hbs2 $out/bin/git-hbs2
    #   '';
    # };

    hpOverridesPre = pkgs: new: old: with pkgs.haskell.lib; {
      scotty = new.callHackage "scotty" "0.21" {};
      skylighting-lucid = new.callHackage "skylighting-lucid" "1.0.4" { };
      bytestring-mmap-compat = new.callHackageDirect {
        pkg = "bytestring-mmap-compat";
        ver = "0.2.3";
        sha256 = "0psd8fc3ryrs3f909hr77c2snckazhy188jy0496ll3402h0fcj1";
      } {};
      wai-app-file-cgi = dontCoverage (dontCheck (jailbreakUnbreak pkgs old.wai-app-file-cgi));
      libyaml =
        if pkgs.hostPlatform.isStatic
          then old.libyaml.overrideDerivation (drv: {
            postPatch = let sed = "${pkgs.gnused}/bin/sed"; in ''
              ${sed} -i -e 's/buffer_init/snoyberg_buffer_init/' c/helper.c include/helper.h
              ${sed} -i -e 's/"buffer_init"/"snoyberg_buffer_init"/' src/Text/Libyaml.hs
            '';
          })
          else old.libyaml;
    };

    overrideComposable = pkgs: hpkgs: overrides:
      hpkgs.override (oldAttrs: {
        overrides = pkgs.lib.composeExtensions (oldAttrs.overrides or (_: _: { })) overrides;
      });

    makePkgsFromDirOverride = pkgs: ov: pkgNames: mkPath:
      pkgs.lib.genAttrs pkgNames (name:
        ov (pkgs.haskellPackages.callCabal2nix name "${self}/${mkPath name}" {})
      );

    makePkgsFromDir = pkgs: makePkgsFromDirOverride pkgs (q: q);
    makePkgsFromDirWithMan = pkgs: makePkgsFromDirOverride pkgs (q:
      q.overrideDerivation (drv: {
          postInstall = ''
            if [ -d man ]; then
              mkdir -p $out
              cp -r man $out/
            fi
          '';
        })
    );

    ourHaskellPackages = pkgs: ({}
      // makePkgsFromDirWithMan pkgs topLevelPackages (n: n)
      // makePkgsFromDirWithMan pkgs keymanPackages (name: "hbs2-keyman/${name}")
      // makePkgsFromDir pkgs miscellaneous (name: "miscellaneous/${name}")
    );

    overlay = final: prev: {
      haskellPackages = overrideComposable prev prev.haskellPackages
        (new: old:
          hpOverridesPre prev new old
            // ourHaskellPackages final
        );
      };

  in
  {
    overlays.default = overlay;
    homeManagerModules.default = import ./nix/hm-module.nix self;
    nixosModules.default = import ./nix/nixos-module.nix self;
  }
  //
  (flake-utils.lib.eachSystem ["x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin"]
  (system:
    let
      pkgs = import nixpkgs {
        inherit system;
        overlays = [overlay];
      };

      packagePostOverrides = pkg: with pkgs.haskell.lib.compose; pkgs.lib.pipe pkg [
        disableExecutableProfiling
        disableLibraryProfiling
        dontBenchmark
        dontCoverage
        dontDistribute
        dontHaddock
        dontHyperlinkSource
        doStrip
        enableDeadCodeElimination
        justStaticExecutables

        dontCheck

        (overrideCabal (drv: {
            preBuild = ''
              export GIT_HASH="${self.rev or self.dirtyRev or "dirty"}"
              '';
             disallowGhcReference = false;
          }))
      ];

    makePackages = pkgs:
      let ps = pkgs.lib.mapAttrs
        (_name: packagePostOverrides) # we can't apply overrides inside our overlay because it will remove linking info
        (pkgs.lib.getAttrs packageNames (ourHaskellPackages pkgs))
        ;
      in ps // {
        bf6-git-hbs2 = pkgs.callPackage ./nix/bf6-hbs2-git.nix { inherit (pkgs.haskellPackages) suckless-conf; };
      };

    packagesDynamic = makePackages pkgs;
    packagesStatic = makePackages pkgs.pkgsStatic;

    # hbs2-peer container image. On x86_64 it is built from the static
    # (musl) binaries so the layer carries no glibc or distro
    # userspace. On aarch64-linux the musl cross-GHC does not build
    # (rts compile errors), so the image falls back to the dynamic
    # (glibc) binaries instead: justStaticExecutables already links the
    # Haskell libs statically, so the dynamic closure only adds glibc
    # plus a handful of system libs (gmp, libsodium, zlib, ...), which
    # nix pulls into the image automatically via store references.
    #
    # The image bundles the full shipped binary set (matches
    # `.#static`) so that `docker exec <name> hbs2-cli ...`,
    # `... hbs2-keyman ...`, and `... git hbs2 ...` work without a
    # second image or host install; this follows the conventional
    # postgres/redis pattern of shipping the admin CLI alongside the
    # daemon. Loadable with `docker load < ./result`.
    #
    # We re-derive each binary via `cp -L` so the image's runtime
    # closure is only the actual binary contents, not the Haskell
    # `lib/` outputs (which reference the compiler and library
    # archives, ~3 GiB per package).
    stripPackageToBin = pkg: pkgs.runCommand "${pkg.pname}-bin" { } ''
      shopt -s nullglob
      mkdir -p $out/bin
      for f in ${pkg}/bin/*; do
        cp -L "$f" $out/bin/
      done
      chmod -R u+w $out 2>/dev/null || true
    '';

    dockerImageFor = imagePackages: pkgs.dockerTools.buildImage {
      name = "hbs2-peer";
      tag = imagePackages.hbs2-peer.version;
      created = "now";

      copyToRoot = pkgs.buildEnv {
        name = "hbs2-image-root";
        # bf6-git-hbs2 is excluded: it is a single-line shebang script
        # `#! /nix/store/...-suckless-conf-static/bin/bf6 file` that
        # pulls suckless-conf-static into the runtime closure, which in
        # turn drags in the full GHC + GCC toolchain (~2.3 GiB). The
        # symlink below provides the `git hbs2 ...` dispatch without
        # the bf6 dependency, mirroring the cabal-install fallback
        # documented in INSTALL.md.
        paths = (map stripPackageToBin (builtins.attrValues
                  (removeAttrs imagePackages [ "bf6-git-hbs2" ]))) ++ [
          (pkgs.runCommand "git-hbs2-symlink" { } ''
            mkdir -p $out/bin
            ln -s hbs2-git3 $out/bin/git-hbs2
          '')
          pkgs.cacert
          pkgs.tzdata
        ];
        pathsToLink = [ "/bin" "/etc" "/share" ];
      };

      config = {
        # No Entrypoint, so `docker run image hbs2-cli ...` works as
        # naturally as `docker run image` (which falls through to Cmd).
        Cmd = [ "/bin/hbs2-peer" "run" ];
        # Defaults from QUICKSTART; operators map -p as needed.
        ExposedPorts = {
          "7351/udp" = { };
          "10351/tcp" = { };
          "5000/tcp" = { };
        };
        # Single volume holds config (~/.config/hbs2-peer), keys
        # (~/.hbs2-keyman/keys/), and storage (~/.local/share/hbs2).
        # HOME=/data routes all three into it.
        Volumes = {
          "/data" = { };
        };
        WorkingDir = "/data";
        Env = [
          "HOME=/data"
          # PATH=/bin lets `docker run image hbs2-cli ...` and
          # `docker exec ... hbs2-cli ...` resolve bare command names.
          "PATH=/bin"
          "SSL_CERT_FILE=/etc/ssl/certs/ca-bundle.crt"
        ];
      };
    };

    in  {
    legacyPackages = pkgs;

    packages =
      packagesDynamic //
      {
        default =
        pkgs.symlinkJoin {
          name = "hbs2-all";
          paths = builtins.attrValues packagesDynamic;
        };
        static =
        pkgs.symlinkJoin {
          name = "hbs2-static";
          paths = builtins.attrValues packagesStatic;
        };
        # See the dockerImageFor comment: aarch64-linux cannot build
        # the musl cross-GHC, so its image ships dynamic binaries.
        docker = dockerImageFor
          (if system == "aarch64-linux" then packagesDynamic else packagesStatic);
      };


    devShells.default = pkgs.haskellPackages.shellFor {
      packages = p: builtins.attrValues (ourHaskellPackages pkgs) ++ [
        p.skylighting-core  # needed for hbs2-tests which we did not expose
      ];
      # withHoogle = true;
      buildInputs = (
        with pkgs.haskellPackages; [
          ghc
          ghcid
          cabal-install
          haskell-language-server
          hoogle
          # htags
          text-icu
          magic
          pkgs.icu72
          pkgs.openssl
          weeder
        ]
        ++
        [ pkgs.pkg-config
          pkgs.libsodium
          pkgs.file
          pkgs.zlib
          pkgs.fuse
          inputs.hspup.packages.${pkgs.system}.default
        ]
      );

      shellHook = ''
      export GIT_HASH="${self.rev or self.dirtyRev or "dirty"}"
      '';

    };
  }
 ));

}

