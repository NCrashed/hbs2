self: {
  config,
  lib,
  pkgs,
  ... }: let
    inherit (pkgs.stdenv.hostPlatform) system;

    package = self.packages.${system}.default;

    cfg = config.services.hbs2;
    hbs2 = cfg.package;
    hbs2-peer = "${hbs2}/bin/hbs2-peer";
in {
  options = {
    services.hbs2 = {
      enable = lib.mkEnableOption "hbs2-peer daemon";
      package = lib.mkOption {
        type = lib.types.package;
        description = "Package with all HBS2 basic binaries";
        default = package;
      };
    };
  };
  config = lib.mkIf cfg.enable {

    home.packages = [ cfg.package ];

    systemd.user.services.hbs2-peer = {
      Unit = {
        Description = "HBS2 peer daemon";
        After = [ "network.target" ];
      };
      Install = {
        WantedBy = [ "default.target" ];
      };
      Service = {
        ExecPreStart = "${hbs2-peer} init";
        ExecStart = "${hbs2-peer} run";
        Restart = "always";
        RuntimeMaxSec = "1d";
      };
    };
  };
}
