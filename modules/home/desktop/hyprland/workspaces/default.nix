{
  config,
  lib,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.desktop.hyprland.workspaces;
in {
  options.${namespace}.desktop.hyprland.workspaces = {
    enable = mkBoolOpt false "Enable per-monitor static workspace mapping";
  };

  config = mkIf cfg.enable {
    wayland.windowManager.hyprland.settings.workspace = [
      # Monitor 1 (z. B. oben oder links)
      "1, monitor:desc:Hewlett Packard HP E232 3CQ7020B20"
      "2, monitor:desc:Hewlett Packard HP E232 3CQ7020B20"
      "3, monitor:desc:Hewlett Packard HP E232 3CQ7020B20"

      # Monitor 2 (z. B. unten oder rechts)
      "1, monitor:desc:Hewlett Packard HP E232 3CQ70218NK"
      "2, monitor:desc:Hewlett Packard HP E232 3CQ70218NK"
      "3, monitor:desc:Hewlett Packard HP E232 3CQ70218NK"
    ];
  };
}
