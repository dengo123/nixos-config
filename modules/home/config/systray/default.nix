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
    after ? [],
    restart ? "on-failure",
  }: {
    "systray-${name}" = {
      Unit = {
        Description = "Systray: ${name}";
        After = ["graphical-session.target" "tray.target"] ++ after;
        Wants = ["tray.target"];
        PartOf = ["graphical-session.target"];
      };
      Service = {
        ExecStart = exec;
        Restart = restart;
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
    startUdiskie = mkBoolOpt true "Start udiskie (automount tray).";
    udiskieArgs = mkOpt types.str "--tray --automount --notify" "Extra args for udiskie.";
    startCopyQ = mkBoolOpt true "Start CopyQ clipboard manager (tray).";
  };

  config = mkIf cfg.enable {
    systemd.user.targets.tray = {
      Unit = {
        Description = "Awesome tray host is ready";
      };
    };

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
        exec = "${pkgs.networkmanagerapplet}/bin/nm-applet";
      }))

      (mkIf cfg.startUdiskie (mkTrayService {
        name = "udiskie";
        exec = "${pkgs.udiskie}/bin/udiskie ${cfg.udiskieArgs}";
      }))

      (mkIf cfg.startCopyQ (mkTrayService {
        name = "copyq";
        exec = "${pkgs.copyq}/bin/copyq";
        restart = "always";
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
        "udiskie.desktop"
        "copyq.desktop"
      ])
    ];
  };
}
