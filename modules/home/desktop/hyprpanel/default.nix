{
  config,
  lib,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.desktop.hyprpanel;
in {
  options.${namespace}.desktop.hyprpanel = {
    enable = mkBoolOpt false "${namespace}.programs.hyprpanel.enable";
  };

  config = mkIf cfg.enable {
    programs.hyprpanel = {
      enable = true;

      settings = {
        layout = {
          "bar.layouts" = {
            "1" = {
              position = "left";
              top = [
                "workspaces"
              ];
              middle = [
                "windowtitle"
              ];
            };
            "0" = {
              left = [
                "dashboard"
                "workspaces"
                "windowtitle"
              ];
              middle = [
                "clock"
              ];
              right = [
                "network"
                "bluetooth"
                "volume"
                "systray"
                "notifications"
              ];
            };
          };
        };

        bar = {
          launcher = {
            icon = "";
            autoDetectIcon = false;
          };

          workspaces = {
            show_icons = false;
            show_numbered = false;
            numbered_active_indicator = "underline";
            workspaces = 5;
          };
        };

        menus = {
          clock = {
            time = {
              military = true;
              hideSeconds = true;
            };
          };
        };

        theme = {
          bar.transparent = true;
          font = {
            name = "CaskaydiaCove Nerd Font";
            size = "14px";
          };
        };
      };
    };
  };
}
