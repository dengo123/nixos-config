{
  config,
  lib,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.desktop.hyprland.theme;
in {
  options.${namespace}.desktop.hyprland.theme = with types; {
    enable = mkBoolOpt true "Enable Hyprland window decoration and theme settings.";
  };

  config = mkIf cfg.enable {
    wayland.windowManager.hyprland.settings = {
      general = {
        gaps_in = 3;
        gaps_out = 5;
        border_size = 1;
        border_part_of_window = true;
        layout = "master";
      };

      decoration = {
        rounding = 10;
        active_opacity = 0.9;
        inactive_opacity = 0.8;

        blur = {
          enabled = true;
        };

        shadow = {
          enabled = true;
          range = 20;
          render_power = 3;
        };
      };
    };
  };
}
