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

      (mkIf (cfg.mode == "vert-1") {
        monitor = [
          "desc:Hewlett Packard HP E232 3CQ7020B20,1920x1080@60,0x0,1,transform,1"
          "desc:Hewlett Packard HP E232 3CQ70218NK,1920x1080@60,1080x420,1"
        ];
      })

      (mkIf (cfg.mode == "vert-2") {
        monitor = [
          "desc:Hewlett Packard HP E232 3CQ7020B20,1920x1080@60,0x420,1"
          "desc:Hewlett Packard HP E232 3CQ70218NK,1920x1080@60,1080x0,1,transform,1"
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
