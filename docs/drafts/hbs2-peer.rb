## Scaffold Homebrew formula for hbs2.
##
## STATUS: draft. Will move to `NCrashed/homebrew-hbs2` repo at
## `Formula/hbs2-peer.rb` once we validate the manual end-to-end
## install on a real Apple Silicon Mac.
##
## After running `scripts/bundle-darwin.sh <version>`, paste the
## sha256 below and upload the tarball as an asset on the matching
## GitHub Release tag.
##
## To install locally without a tap:
##   brew install --formula ./docs/drafts/hbs2-peer.rb
##
## Once published to the tap:
##   brew install ncrashed/hbs2/hbs2-peer

class Hbs2Peer < Formula
  desc "Hash-addressed distributed storage peer"
  homepage "https://github.com/NCrashed/hbs2"

  version "0.25.5.0"
  license "BSD-3-Clause"

  # git is a RUNTIME dependency and not only a build one: hbs2-git3 and
  # git-remote-hbs23 are git plumbing that git invokes, and hbs2-hub reads
  # a repository's issue tracker by running git. Without it this installs
  # a forge CLI that can announce a verb and not run it, which is the same
  # hole the docker image had.
  depends_on "git"

  # Apple Silicon only. The Intel Mac userbase no longer warrants a
  # separate build leg; Intel users can fall back to `nix run
  # github:NCrashed/hbs2#hbs2-peer` or run Docker Desktop.
  on_macos do
    on_arm do
      url "https://github.com/NCrashed/hbs2/releases/download/0.25.5.0/hbs2-0.25.5.0-aarch64-apple-darwin.tar.gz"
      sha256 "REPLACE_WITH_SHA256_FROM_BUNDLE_SCRIPT"
    end
  end

  def install
    bin.install Dir["bin/*"]
    lib.install Dir["lib/*"]
    pkgshare.install "README.txt"
  end

  def caveats
    <<~EOS
      hbs2-sync's "mount" subcommand requires macFUSE, available at:
        https://osxfuse.github.io/
      All other hbs2-sync subcommands work without it.

      First-run setup:
        hbs2-peer init
      Configuration lives in ~/.config/hbs2-peer/.

      To run hbs2-peer as a background service via launchd:
        brew services start hbs2-peer

      Logs:
        #{var}/log/hbs2-peer.log
    EOS
  end

  service do
    run [opt_bin/"hbs2-peer", "run"]
    keep_alive true
    log_path var/"log/hbs2-peer.log"
    error_log_path var/"log/hbs2-peer.log"
    working_dir Dir.home
  end

  test do
    # `hbs2-peer version` prints a JSON-encoded build version blob.
    # Just verify the binary runs and exits cleanly; full schema
    # assertions belong in the project's own test suite.
    system "#{bin}/hbs2-peer", "version"
  end
end
