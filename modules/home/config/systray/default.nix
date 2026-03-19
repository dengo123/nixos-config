# modules/home/config/systray/default.nix
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

  mkTrayService = {
    name,
    exec,
    after ? ["graphical-session.target"],
  }: {
    "systray-${name}" = {
      Unit = {
        Description = "Systray: ${name}";
        After = after;
        PartOf = ["graphical-session.target"];
      };
      Service = {
        ExecStart = exec;
        Restart = "on-failure";
        RestartSec = 2;
      };
      Install = {
        WantedBy = ["graphical-session.target"];
      };
    };
  };

  mkHiddenDesktop = n: ''
    [Desktop Entry]
    Type=Application
    Name=${n} (disabled)
    Hidden=true
    X-GNOME-Autostart-enabled=false
    NotShowIn=Awesome;
  '';

  hiddenFilesFrom = names:
    builtins.listToAttrs (
      map (n: {
        name = ".config/autostart/${n}";
        value.text = mkHiddenDesktop n;
      })
      names
    );
in {
  options.${namespace}.config.systray = with types; {
    enable = mkBoolOpt false "Systray applets via systemd --user; XDG autostarts disabled.";
    startBlueman = mkBoolOpt true "Start Blueman applet (bluetooth) with systemd.";
    startPasystray = mkBoolOpt true "Start pasystray (audio) with systemd.";
    startNmApplet = mkBoolOpt true "Start nm-applet (network) with systemd.";
  };

  config = mkIf cfg.enable {
    systemd.user.services = mkMerge [
      (mkIf cfg.startBlueman (mkTrayService {
        name = "blueman";
        exec = "${pkgs.blueman}/bin/blueman-applet";
      }))

      (mkIf cfg.startPasystray (mkTrayService {
        name = "pasystray";
        exec = "${pkgs.pasystray}/bin/pasystray";
      }))

      (mkIf cfg.startNmApplet (mkTrayService {
        name = "nm-applet";
        exec = "${pkgs.networkmanagerapplet}/bin/nm-applet --indicator";
      }))
    ];

    home.file = mkMerge [
      (hiddenFilesFrom [
        "pasystray.desktop"
        "blueman.desktop"
        "blueman-applet.desktop"
        "blueberry.desktop"
        "blueberry-tray.desktop"
        "nm-applet.desktop"
      ])
    ];
  };
}
