{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.bundles.desktop.hyprland;
in {
  options.${namespace}.bundles.desktop.hyprland = with types; {
    enable = mkBoolOpt false "Whether or not to enable desktop hyprland bundle configuration.";
  };

  config = mkIf cfg.enable {
    # Fallbacks für animation/monitor mode (nur gesetzt, falls nicht gesetzt)
    nixforge.desktop.hyprland = {
      animation = mkDefault "medium";
      monitor.mode = mkDefault "auto-script";
      inputs = {
        layout = mkDefault "us";
        variant = mkDefault "altgr-intl";
        options = mkDefault [];
      };
    };

    home.packages = with pkgs; [
      adwaita-icon-theme
      brightnessctl
      gnome-system-monitor
      gnome-control-center
      morewaita-icon-theme
      pavucontrol
      swww
      qogir-icon-theme
      wayshot
      wl-clipboard
      wl-gammactl
    ];

    nixforge = {
      desktop.hyprland.enable = true;
      programs = {
        ags = enabled;
        hypridle = enabled;
        hyprpaper = enabled;
        rofi = enabled;
        # hyprlock = disabled; # kannst du aktivieren wenn gewünscht
        # dolphin = disabled;
        # waybar = disabled;
      };
    };
  };
}
