{
  lib,
  config,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.desktop.hyprland.plugins.hyprsplit;
in {
  options.${namespace}.desktop.hyprland.plugins.hyprsplit = with types; {
    enable = mkBoolOpt false "Enable the hyprsplit plugin for Hyprland";
  };

  config = mkIf cfg.enable {
    wayland.windowManager.hyprland = {
      plugins = [
        pkgs.hyprlandPlugins.hyprsplit
      ];

      settings.plugin.hyprsplit = {
        num_workspaces = 3;
        persistent_workspaces = true;
      };
    };
  };
}
