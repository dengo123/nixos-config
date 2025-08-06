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
    nixforge.desktop.hyprland = {
      enable = true;
      animation = mkDefault "medium";
      monitor.mode = mkDefault "auto-script";
      inputs = {
        layout = mkDefault "us";
        variant = mkDefault "altgr-intl";
        options = mkDefault [];
      };
      plugins = {
        hyprsplit = enabled;
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

    services.blueman-applet.enable = true;

    nixforge = {
      programs = {
        hyprpaper = enabled;
        rofi = enabled;
        hyprlock = enabled;
        swaync = enabled;
        waybar = enabled;
      };
      services = {
        hypridle = enabled;
        kanshi = enabled;
        autorandr = enabled;
      };
    };
  };
}
