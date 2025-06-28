{
  config,
  lib,
  inputs,
  ...
}: {
  options.nixforge.desktop.hyprland.plugins.split-monitor-workspaces = {
    enable = lib.mkEnableOption "Enable the split-monitor-workspaces Hyprland plugin";
  };

  config = lib.mkIf config.nixforge.desktop.hyprland.plugins.split-monitor-workspaces.enable {
    wayland.windowManager.hyprland.plugins = [
      "${inputs.split-monitor-workspaces}/lib/libsplit-monitor-workspaces.so"
    ];
  };
}
