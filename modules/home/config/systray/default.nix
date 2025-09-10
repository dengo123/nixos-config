{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.config.systray;
in {
  options.${namespace}.config.systray = with types; {
    enable = mkBoolOpt false "Enable system tray applets autostart.";
    autostartTarget = mkOpt str "default.target" "Target for user services.";

    startBluetoothApplet = mkBoolOpt false "Autostart blueman-applet.";
    startNmApplet = mkBoolOpt false "Autostart nm-applet.";
    startPasystray = mkBoolOpt false "Autostart pasystray.";
  };

  config = mkIf cfg.enable {
    # blueman-applet
    systemd.user.services.blueman-applet = mkIf cfg.startBluetoothApplet {
      Unit = {
        Description = "Blueman Applet";
        After = [cfg.autostartTarget];
        PartOf = [cfg.autostartTarget];
      };
      Service = {
        ExecStart = "${pkgs.blueman}/bin/blueman-applet";
        Restart = "on-failure";
        RestartSec = 2;
      };
      Install = {
        WantedBy = [cfg.autostartTarget];
      };
    };

    # nm-applet
    systemd.user.services.nm-applet = mkIf cfg.startNmApplet {
      Unit = {
        Description = "NetworkManager Applet";
        After = [cfg.autostartTarget];
        PartOf = [cfg.autostartTarget];
      };
      Service = {
        ExecStart = "${pkgs.networkmanagerapplet}/bin/nm-applet";
        Restart = "on-failure";
        RestartSec = 2;
      };
      Install = {
        WantedBy = [cfg.autostartTarget];
      };
    };

    # pasystray
    systemd.user.services.pasystray = mkIf cfg.startPasystray {
      Unit = {
        Description = "PulseAudio System Tray (pasystray)";
        After = [cfg.autostartTarget];
        PartOf = [cfg.autostartTarget];
      };
      Service = {
        ExecStart = "${pkgs.pasystray}/bin/pasystray";
        Restart = "on-failure";
        RestartSec = 2;
      };
      Install = {
        WantedBy = [cfg.autostartTarget];
      };
    };
  };
}
