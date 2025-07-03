{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.programs.hyprpanel;
in {
  options.${namespace}.programs.hyprpanel = {
    enable = mkBoolOpt false "${namespace}.programs.hyprpanel.enable";
  };

  config = mkIf cfg.enable {
    programs.hyprpanel = {
      enable = true;

      layout = {
        "bar.layouts" = {
          "0" = {
            left = [
              "custom"
              "workspaces"
            ];
            middle = ["clock"];
            right = [
              "network"
              "volume"
              "battery"
              "systray"
            ];
          };
        };
      };

      settings = {
        bar = {
          launcher.autoDetectIcon = true;
          workspaces = {
            show_icons = false;
            format = "●";
          };
        };

        menus.clock = {
          time = {
            military = true;
            hideSeconds = true;
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
