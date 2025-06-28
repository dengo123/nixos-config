{
  config,
  lib,
  inputs,
  system,
  ...
}: let
  cfg = config.nixforge.desktop.hyprland.plugins.split-monitor-workspaces;

  pluginPkg = inputs.self.packages.${system}.split-monitor-workspaces;
in {
  options.nixforge.desktop.hyprland.plugins.split-monitor-workspaces = {
    enable = lib.mkEnableOption "Enable split-monitor-workspaces plugin";
  };

  config = lib.mkIf cfg.enable {
    wayland.windowManager.hyprland.plugins = [
      "${pluginPkg}/lib/libsplit-monitor-workspaces.so"
    ];
  };
}
