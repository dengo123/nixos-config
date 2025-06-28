{
  config,
  lib,
  pkgs,
  ...
}: let
  pluginPkg = config.nixforge.desktop.hyprland.plugins.split-monitor-workspaces.package;
in {
  options.nixforge.desktop.hyprland.plugins.split-monitor-workspaces = {
    enable = lib.mkEnableOption "Enable the split-monitor-workspaces Hyprland plugin";
    package = lib.mkOption {
      type = lib.types.package;
      description = "The package providing the plugin shared object.";
    };
  };

  config = lib.mkIf config.nixforge.desktop.hyprland.plugins.split-monitor-workspaces.enable {
    systemd.user.services.hyprland-plugin-split-monitor-workspaces = {
      Unit = {
        Description = "Load Hyprland Plugin: split-monitor-workspaces";
        After = ["graphical-session.target"];
        Requires = ["graphical-session.target"];
      };

      Service = {
        ExecStart = "${pkgs.hyprland}/bin/hyprctl plugin load ${pluginPkg}/lib/libsplit-monitor-workspaces.so";
        Restart = "on-failure";
        RestartSec = 2;
      };

      Install.WantedBy = ["graphical-session.target"];
    };
  };
}
