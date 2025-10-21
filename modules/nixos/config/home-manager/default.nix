{
  pkgs,
  config,
  lib,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.programs.home-manager;
in {
  options.${namespace}.programs.home-manager = with types; {
    enable = mkBoolOpt true "Enable Home Manager as system service";
    extraOptions = mkOpt attrs {} "${namespace}.programs.home-manager.extraOptions";
  };

  config = mkIf cfg.enable {
    home-manager = {
      useUserPackages = true;
      backupFileExtension = "backup"; # <- korrekt global gesetzt
    };

    snowfallorg.users.${config.${namespace}.config.user.name}.home.config = cfg.extraOptions;

    systemd.services.cleanup-home-manager-backups = {
      description = "Clean up old .backup files before Home Manager runs";
      before = ["home-manager-${config.${namespace}.config.user.name}.service"];
      wantedBy = ["multi-user.target"];

      serviceConfig = {
        Type = "oneshot";
        ExecStart = lib.mkForce (
          pkgs.writeShellScript "cleanup-hm-backups" ''
            find "${
              config.users.users.${config.${namespace}.config.user.name}.home
            }" -type f -name '*.backup' -delete
          ''
        );
      };
    };
  };
}
