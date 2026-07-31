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
      "hbs2-hub"
      ];

    miscellaneous =
      [
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
      # These forks now declare lower bounds wide enough for the static
      # (pkgsStatic) GHC 9.4.8 toolchain (mtl 2.2, transformers 0.5), so
      # no jailbreak is needed. See each fork's CHANGELOG / per-fork CI
      # (GHC 9.4.8/9.6.6/9.8) for the bound rationale.
      bytestring-mmap-compat = new.callHackageDirect {
        pkg = "bytestring-mmap-compat";
        ver = "0.2.3";
        sha256 = "0psd8fc3ryrs3f909hr77c2snckazhy188jy0496ll3402h0fcj1";
      } {};
      db-pipe = new.callHackageDirect {
        pkg = "db-pipe";
        ver = "0.1.0.2";
        sha256 = "sha256-9x+uyP2OWpHrvPyfccCXkbtmzJ/g2XJXd8EvrrHvTWY=";
      } {};
      suckless-conf = new.callHackageDirect {
        pkg = "suckless-conf";
        ver = "0.1.2.12";
        sha256 = "sha256-ZKrN8cHLmUI/oYJjwct+NM911spq5Olz/T2Z1k5OyBk=";
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

    # git for hbs2-hub's test suite, in the OVERLAY and not only in the devShell.
    #
    # The shipped packages are all dontCheck, so nix never runs the suite for
    # them; the overlay this flake exports is not, and anybody building
    # haskellPackages.hbs2-hub through it runs the suite in a sandbox with no git,
    # where GitRepoSpec's twenty-one examples turn into pendingWith and hspec
    # calls that a pass. A green run that tested none of the git-facing half.
    withGitForTests = pkgs: p:
      pkgs.haskell.lib.addTestToolDepends p [ pkgs.gitMinimal ];

    ourHaskellPackages = pkgs: (
      let base = {}
            // makePkgsFromDirWithMan pkgs topLevelPackages (n: n)
            // makePkgsFromDirWithMan pkgs keymanPackages (name: "hbs2-keyman/${name}")
            // makePkgsFromDir pkgs miscellaneous (name: "miscellaneous/${name}");
      in base // { hbs2-hub = withGitForTests pkgs base.hbs2-hub; }
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

    # hbs2-peer container image. On x86_64 the hbs2 binaries are the static
    # (musl) ones, so none of THEM carries glibc or distro userspace. The
    # image as a whole does: pkgs.gitMinimal below comes from the dynamic
    # pkgs, and brings glibc with it. That is deliberate (see the note
    # beside it) and it is why this no longer claims a glibc-free image.
    #
    # On aarch64-linux the musl cross-GHC does not build
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

    # PEP-22 spells the forge CLI `hub`; the binary is named hbs2-hub like
    # everything else here, because `hub` is also a widely installed GitHub tool
    # and claiming that name in PATH is not ours to do. The symlink offers the
    # documented surface to whoever wants it.
    #
    # In every output that ships binaries, not just the docker image: the linux
    # tarball is built from `.#static` and a symlink added only to the image
    # would have left `nix profile install .#` and the release archive without
    # it. git-hbs2 escapes the same gap only because bf6-git-hbs2 brings a
    # same-named script along.
    hubAlias = pkgs.runCommand "hbs2-hub-alias" { } ''
      mkdir -p $out/bin
      ln -s hbs2-hub $out/bin/hub
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
          pkgs.cacert
          pkgs.tzdata
          # git, because the image already ships git-hbs2 and hbs2-git3, and
          # hbs2-hub reads canon out of an ordinary git ref by running git. A
          # forge CLI in an image with no git can announce a verb and not run it.
          #
          # gitMinimal and not git: measured against the nixpkgs THIS FLAKE PINS
          # (nix path-info -S --store https://cache.nixos.org on the two store
          # paths its legacyPackages give), the full package's closure is 326 MB
          # against gitMinimal's 123 MB, and the difference is perl with 35
          # modules and a python, in an image built from packagesStatic precisely
          # so that the hbs2 binaries carry no glibc. git does bring glibc into the
          # image, which is the price of shipping a forge CLI that shells out;
          # gitMinimal keeps that price to one libc instead of a perl and a
          # python. What is used here is rev-parse, show-ref, ls-tree and cat-file.
          #
          # Measured on the pin and not on whatever nixpkgs is on the developer's
          # path, which is where the previous numbers (402/167 MB, 39 modules)
          # came from: they were true of a different nixpkgs than the one this
          # builds with.
          pkgs.gitMinimal
        ];
        pathsToLink = [ "/bin" "/etc" "/share" ];

        # The two aliases are made HERE and not brought in as a path, which is
        # what hubAlias and a git-hbs2-symlink derivation used to be. buildEnv
        # does not copy a symlink, it links to it, so a relative `hub -> hbs2-hub`
        # inside an alias derivation became
        # /bin/hub -> /nix/store/...-alias/bin/hub -> hbs2-hub, resolved next to
        # the alias, where there is no hbs2-hub: a dangling link, in the one
        # channel this whole stage put git into. symlinkJoin (used by .#default
        # and .#static) keeps them relative and was fine, which is why the two
        # smoke tests added last round did not see it.
        postBuild = ''
          ln -sf hbs2-hub $out/bin/hub
          ln -sf hbs2-git3 $out/bin/git-hbs2
        '';
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
          # No wrapper putting git on hbs2-hub's PATH, though it is tempting.
          # This output is what scripts/bundle-darwin.sh builds and copies with
          # `cp -L`, so a wrapper arrives in the tarball and in the Homebrew
          # bundle as a 414-byte shell script naming /nix/store paths that do not
          # exist on the target machine. It also would not cover `nix run
          # .#hbs2-hub` or `nix profile install .#hbs2-hub`, which take the
          # package and not this join. git is a documented requirement instead
          # (INSTALL.md), which is the same answer every non-nix install gets.
          paths = builtins.attrValues packagesDynamic ++ [ hubAlias ];
        };
        static =
        pkgs.symlinkJoin {
          name = "hbs2-static";
          paths = builtins.attrValues packagesStatic ++ [ hubAlias ];
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
          # git, because hbs2-hub's test suite runs it: canon is a git ref, and
          # the half of the reader that talks to git is tested against real git
          # in a temp repository rather than against a mock of the format.
          pkgs.gitMinimal
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

