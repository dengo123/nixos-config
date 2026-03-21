# modules/home/bundles/desktop/awesome/default.nix
{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.bundles.desktop.awesome;
in {
  options.${namespace}.bundles.desktop.awesome = with types; {
    enable = mkBoolOpt false "Whether or not to enable desktop awesome bundle configuration.";
  };

  config = mkIf cfg.enable {
    nixforge = {
      misc = {
        gtk = {
          enable = true;
          iconTheme = "Papirus-Dark"; # or "Adwaita" | "Papirus-Dark"
        };

        scripts = {
          autorandr-toggle = enabled;
        };
      };

      config = {
        systray = {
          enable = true;
        };
      };

      services = {
        xscreensaver = {
          enable = true;
          suspend = {
            enable = true;
            secondsToSuspend = 1500;
          };
        };

        picom = {
          enable = true;
          # fade = true;
          # shadow = true;
          # inactiveOpacity = 0.9;
        };

        redshift = {
          enable = true;
          provider = "manual";
          latitude = 50.1;
          longitude = 8.6;
        };

        udiskie = enabled;
      };

      programs = {
        nemo = {
          enable = true;
          withBundle = false;
        };

        autorandr = enabled;
      };
    };

    home.packages = with pkgs; [
      copyq
      gnome-calendar
      gowall
      imagemagick
    ];
  };
}
