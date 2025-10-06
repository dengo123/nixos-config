# modules/home/services/xidlehook/default.nix
{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.services.xidlehook;
in {
  options.${namespace}.services.xidlehook = with types; {
    enable = mkBoolOpt false "Enable xidlehook-based idle actions.";
    # Optional: xscreensaver als Visual-Idle-Daemon dazu starten
    useXSS = mkBoolOpt false "Also enable xscreensaver; xidlehook triggers xscreensaver-command -activate.";
  };

  config = mkIf cfg.enable {
    # 1) xscreensaver optional aktivieren (ohne weitere Voreinstellungen)
    services.xscreensaver = mkIf cfg.useXSS {
      enable = true;
      package = pkgs.xscreensaver;
    };

    # 2) xidlehook: nacktes Setup, nur die drei gewünschten Timer
    services.xidlehook = {
      enable = true;
      timers =
        (optional cfg.useXSS {
          # nach 5 Minuten: xscreensaver einschalten (falls xscreensaver läuft)
          delay = 300; # Sekunden
          command = "${pkgs.xscreensaver}/bin/xscreensaver-command -activate";
        })
        ++ [
          # nach 10 Minuten: LightDM sperren
          {
            delay = 600;
            command = "${pkgs.lightdm}/bin/dm-tool lock";
          }
          # nach 30 Minuten: System suspend
          {
            delay = 1800;
            command = "${pkgs.systemd}/bin/systemctl suspend";
          }
        ];
      # Keine weiteren Defaults (kein notWhenFullscreen, detectSleep, etc.)
    };
  };
}
