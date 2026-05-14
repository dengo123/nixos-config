# modules/home/services/xscreensaver/default.nix
{
  config,
  lib,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.services.xscreensaver;
in {
  options.${namespace}.services.xscreensaver = with types; {
    enable = mkBoolOpt false "Enable XScreenSaver daemon.";
  };

  config = mkIf cfg.enable {
    services.xscreensaver = {
      enable = true;
    };

    systemd.user.services.xscreensaver = {
      Service = {
        Environment = [
          "DISPLAY=:0"
          "XAUTHORITY=%h/.Xauthority"
          "XDG_RUNTIME_DIR=/run/user/%U"
          "DBUS_SESSION_BUS_ADDRESS=unix:path=/run/user/%U/bus"
        ];
        Restart = "on-failure";
        RestartSec = 2;
      };
    };
  };
}
