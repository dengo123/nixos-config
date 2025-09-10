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
  }: {
    "systray-${name}" = {
      Unit = {
        Description = "Systray: ${name}";
        After = ["graphical-session.target"];
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
        value = {
          text = mkHiddenDesktop n;
        };
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
    # 1) systemd --user services (einziger Startpfad)
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

    # 2) Alle möglichen XDG-Autostarts wegdrücken (damit nix doppelt startet)
    home.file = mkMerge [
      (hiddenFilesFrom [
        # Audio
        "pasystray.desktop"
        # Bluetooth – wir nutzen Blueman, also Blueberry & evtl. Blueman-XDG unterdrücken
        "blueberry.desktop"
        "blueberry-tray.desktop"
        "blueman.desktop"
        "blueman-applet.desktop"
        # Netzwerk, falls du systemd für nm-applet nutzt
        "nm-applet.desktop"
      ])
    ];
  };
}
