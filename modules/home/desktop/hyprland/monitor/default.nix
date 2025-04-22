{
  config,
  lib,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.desktop.hyprland.monitor;
in {
  options.${namespace}.desktop.hyprland.monitor = with types; {
    mode = mkStrOpt "auto-script" "Monitor layout mode (e.g. single, dual, laptop, auto-script)";
  };

  config = {
    wayland.windowManager.hyprland.settings = mkMerge [
      # === fixed monitor setups ===
      (mkIf (cfg.mode == "dual") {
        monitor = [
          "DP-1,1920x1080@60,0x0,1"
          "DP-2,1920x1080@60,1920x0,1"
        ];
      })

      (mkIf (cfg.mode == "single") {
        monitor = [
          "DP-1,1920x1080@60,0x0,1"
        ];
      })

      (mkIf (cfg.mode == "laptop") {
        monitor = [
          "eDP-1,preferred,auto,1"
        ];
      })

      # === auto monitor script ===
      (mkIf (cfg.mode == "auto-script") {
        exec-once = ["auto-monitors"];
      })
    ];
  };
}
