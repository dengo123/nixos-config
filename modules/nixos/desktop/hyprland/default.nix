{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.desktop.hyprland;
in {
  options.${namespace}.desktop.hyprland = with types; {
    enable = mkBoolOpt false "Enable Hyprland as a desktop environment.";
    mode = mkStrOpt "full" "Hyprland configuration mode: full or minimal";

    settings = {
      upower = mkBoolOpt false "Enable upower (recommended on laptops)";
    };
  };

  config = mkIf cfg.enable {
    programs.hyprland.enable = true;

    environment.sessionVariables.WLR_NO_HARDWARE_CURSORS = "1";

    xdg.portal = {
      enable = true;
      wlr.enable = true;
      extraPortals = with pkgs; [
        xdg-desktop-portal-gtk
        xdg-desktop-portal-hyprland
      ];
    };

    services = {
      # === COMMON ===
      gvfs.enable = mkIf (cfg.mode == "full") true;
      devmon.enable = mkIf (cfg.mode == "full") true;
      upower.enable = cfg.settings.upower;

      # === DISPLAY MANAGER ===
      xserver.displayManager.gdm = mkIf (cfg.mode == "full") {
        enable = true;
        wayland = true;
      };
    };

    systemd.user.services.polkit-gnome-authentication-agent-1 = mkIf (cfg.mode == "full") {
      description = "polkit-gnome-authentication-agent-1";
      wantedBy = ["graphical-session.target"];
      wants = ["graphical-session.target"];
      after = ["graphical-session.target"];
      serviceConfig = {
        Type = "simple";
        ExecStart = "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1";
        Restart = "on-failure";
        RestartSec = 1;
        TimeoutStopSec = 10;
      };
    };

    ${namespace} = {
      programs = mkIf (cfg.mode == "full") {
        dconf = enabled;
        kde-connect = enabled;
        nautilus = enabled;
      };

      services = mkMerge [
        (mkIf (cfg.mode == "full") {
          polkit-gnome = enabled;
          greetd = disabled;
        })
        (mkIf (cfg.mode == "minimal") {
          greetd = enabled;
        })
      ];
    };
  };
}
