# modules/nixos/services/light-locker/default.nix
{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.services.lightlocker;
in {
  options.${namespace}.services.lightlocker = {
    enable = mkBoolOpt false "Start light-locker so LightDM is used as the unlock greeter when the X11 screensaver activates.";
  };

  config = mkIf cfg.enable {
    # User-Unit: startet NACH der grafischen Session, ohne Extra-Flags
    systemd.user.services.light-locker = {
      description = "light-locker (LightDM lock after X11 screensaver)";
      after = ["graphical-session.target"];
      partOf = ["graphical-session.target"];
      wantedBy = ["graphical-session.target"];
      serviceConfig = {
        # Start in einer Login-Shell, damit die X-Env der Session geerbt wird
        ExecStart = "${pkgs.bash}/bin/bash -lc 'exec ${pkgs.lightlocker}/bin/light-locker'";
        Restart = "on-failure";
        RestartSec = 2;
      };
    };
  };
}
