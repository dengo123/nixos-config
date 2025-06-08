{
  lib,
  config,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.programs.swaync;
  stylix = config.lib.stylix.colors;
in {
  options.${namespace}.programs.swaync = with types; {
    enable = mkBoolOpt false "Enable swaynotificationcenter and related setup";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      swaynotificationcenter
      wlsunset
      grimblast
      swappy
    ];

    services.swaync = {
      enable = true;
      settings = {
        "$schema" = "/etc/xdg/swaync/configSchema.json";

        positionX = "right";
        positionY = "top";
        cssPriority = "user";

        control-center-margin-top = 22;
        control-center-margin-bottom = 2;
        control-center-margin-right = 1;
        control-center-margin-left = 0;

        notification-icon-size = 64;
        notification-body-image-height = 128;
        notification-body-image-width = 200;

        timeout = 6;
        timeout-low = 3;
        timeout-critical = 0;

        fit-to-screen = false;
        control-center-width = 400;
        control-center-height = 915;
        notification-window-width = 375;

        keyboard-shortcuts = true;
        image-visibility = "when-available";
        transition-time = 200;
        hide-on-clear = false;
        hide-on-action = true;
        script-fail-notify = true;

        widgets = [
          "title"
          "dnd"
          "menubar#desktop"
          "volume"
          "mpris"
          "notifications"
        ];

        widget-config = {
          title = {
            text = " Quick settings";
            clear-all-button = true;
            button-text = "";
          };

          "menubar#desktop" = {
            "menu#screenshot" = {
              label = "\t󰄀   Screenshot\t";
              position = "left";
              actions = [
                {
                  label = "Whole screen";
                  command = "sh -c 'swaync-client -cp; sleep 1; grimblast copysave output \"/tmp/screenshot.png\"; swappy -f \"/tmp/screenshot.png\"'";
                }
                {
                  label = "Whole window / Select region";
                  command = "sh -c 'swaync-client -cp; grimblast copysave area \"/tmp/screenshot.png\"; swappy -f \"/tmp/screenshot.png\"'";
                }
              ];
            };

            "menu#power" = {
              label = "\t   Power Menu\t  ";
              position = "left";
              actions = [
                {
                  label = "   Logout";
                  command = "hyprctl dispatch exit 0";
                }
                {
                  label = "   Shut down";
                  command = "systemctl poweroff";
                }
                {
                  label = "󰤄   Suspend";
                  command = "systemctl suspend";
                }
                {
                  label = "   Reboot";
                  command = "systemctl reboot";
                }
              ];
            };
          };

          volume = {
            label = "";
            expand-button-label = "";
            collapse-button-label = "";
            show-per-app = true;
            show-per-app-icon = true;
            show-per-app-label = true;
          };

          dnd = {text = " Do Not Disturb";};

          mpris = {
            image-size = 96;
            image-radius = 4;
          };

          label = {
            text = "Notifications";
            clear-all-button = true;
            button-text = "";
          };
        };

        scripts = {
          example-script = {
            exec = "echo 'Do something...'";
            urgency = "Normal";
          };
        };

        notification-visibility = {
          spotify = {
            state = "enabled";
            urgency = "Low";
            app-name = "Spotify";
          };
        };
      };
    };
  };
}
