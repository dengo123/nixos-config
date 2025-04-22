{
  options,
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
    enable = mkBoolOpt false "Whether or not to enable the full Hyprland desktop bundle.";
  };

  config = mkIf cfg.enable {
    # Hyprland + Submodule Features
    nixforge.desktop.hyprland = {
      enable = true;
      animation = mkDefault "medium";
      monitor.mode = mkDefault "auto-script";
    };

    nixforge.programs = {
      ags = enabled;
      rofi = enabled;
      hyprpaper = enabled;
      hypridle = enabled;
      hyprlock = enabled; # optional
    };

    # Core UI/Wayland packages
    home.packages = with pkgs; [
      qt5.qtwayland
      qt6.qtwayland
      libsForQt5.qt5ct
      qt6ct

      brightnessctl
      gnome-system-monitor
      gnome-control-center
      gnome-themes-extra
      morewaita-icon-theme
      adwaita-icon-theme
      qogir-icon-theme

      hyprshot
      hyprpicker
      swappy
      imv
      wf-recorder
      wlr-randr
      wl-clipboard
      wl-gammactl
      direnv
      meson
      wayland-utils
      wayland-protocols
    ];
  };
}
