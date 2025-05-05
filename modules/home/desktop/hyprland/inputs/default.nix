{
  config,
  lib,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.desktop.hyprland.inputs;
in {
  options.${namespace}.desktop.hyprland.inputs = with types; {
    layout = mkOpt str null "Keyboard layout (XKB)";
    variant = mkOpt str null "Keyboard variant (XKB)";
    options = mkOpt (listOf str) [] "List of XKB options";
  };

  config = mkIf (cfg.layout != null && cfg.variant != null) {
    wayland.windowManager.hyprland.settings.input = {
      kb_layout = cfg.layout;
      kb_variant = cfg.variant;
      kb_options = lib.concatStringsSep "," cfg.options;
      follow_mouse = 1;
      sensitivity = 0.0;
      repeat_delay = 300;
      repeat_rate = 50;
      natural_scroll = true;
    };
  };
}
