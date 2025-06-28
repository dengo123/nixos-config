{
  config,
  lib,
  pkgs,
  inputs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.desktop.hyprland.plugins.split-monitor;
in {
  options.${namespace}.desktop.hyprland.plugins.split-monitor = with types; {
    enable = mkBoolOpt false "Enable the split-monitor-workspaces plugin for Hyprland.";
  };

  config = mkIf cfg.enable {
    programs.hyprland.plugins = [
      (pkgs.callPackage ../../../../../packages/hyprland-plugins/split-monitor {
        inherit (inputs) split-monitor-workspaces hyprland;
      })
    ];
  };
}
