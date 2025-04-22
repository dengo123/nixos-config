{
  config,
  lib,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.desktop.hyprland;
  keyboardCfg = config.${namespace}.system.keyboard;
in {
  config = mkIf cfg.enable {
    wayland.windowManager.hyprland.settings.input = {
      kb_layout = keyboardCfg.layout;
      kb_variant = keyboardCfg.variant;
      kb_options = lib.concatStringsSep "," keyboardCfg.options;

      follow_mouse = 1;
      sensitivity = 0.0;
      repeat_delay = 300;
      repeat_rate = 50;

      touchpad = {
        natural_scroll = true;
        tap-to-click = true;
      };
    };
  };
}
