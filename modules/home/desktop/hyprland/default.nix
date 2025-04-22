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
    enable = mkBoolOpt false "Enable Hyprland as Wayland Compositor";

    animation = mkStrOpt "medium" "Animation style (slow, medium, fast)";
    monitor = mkStrOpt "single" "Monitor layout (single, dual, vertical)";
    input = mkBoolOpt true "Enable default input settings";
    theme = mkBoolOpt true "Enable theming (colors, borders, etc.)";
    behavior = mkBoolOpt true "Enable Hyprland window behavior";
    keybindings = mkBoolOpt true "Enable keybindings";
  };

  config = mkIf cfg.enable {
    wayland.windowManager.hyprland = {
      enable = true;
      xwayland.enable = true;
      systemd.enable = true;
    };

    # Default Tooling für Hyprland-Umgebungen
    home.packages = with pkgs; [
      qt5.qtwayland
      qt6.qtwayland
      libsForQt5.qt5ct
      qt6ct
      hyprshot
      hyprpicker
      swappy
      imv
      wf-recorder
      wlr-randr
      wl-clipboard
      brightnessctl
      gnome-themes-extra
      libva
      dconf
      wayland-utils
      wayland-protocols
      direnv
      meson
    ];
  };
}
