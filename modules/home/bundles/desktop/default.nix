# modules/home/bundles/desktop/awesome/default.nix
{
  inputs,
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.bundles.desktop;
in {
  options.${namespace}.bundles.desktop = with types; {
    enable = mkBoolOpt true "Whether or not to enable desktop awesome bundle configuration.";
  };

  config = mkIf cfg.enable {
    nixforge = {
      misc = {
        gtk = {
          enable = mkDefault true;
          iconTheme = mkDefault "Papirus-Dark"; # or "Adwaita" | "Papirus-Dark"
        };

        scripts.enable = mkDefault true;
      };

      config = {
        systray = {
          enable = mkDefault true;
        };
      };

      services = {
        xscreensaver = {
          enable = mkDefault true;
          suspend = {
            enable = mkDefault true;
            secondsToSuspend = mkDefault 1500;
          };
        };

        picom = {
          enable = mkDefault true;
          configFile = mkDefault (inputs.self + /dotfiles/picom/.config/picom/picom.conf);
          manageConfig = mkDefault true;
        };

        redshift = {
          enable = mkDefault true;
          provider = mkDefault "manual";
          latitude = mkDefault 50.1;
          longitude = mkDefault 8.6;
        };

        udiskie.enable = mkDefault true;
      };

      programs = {
        nemo = {
          enable = mkDefault true;
          withBundle = mkDefault false;
        };

        autorandr = mkDefault enabled;
      };
    };

    home.packages = with pkgs; [
      copyq
      gnome-calendar
      gowall
      imagemagick
      lua52Packages.cjson
    ];
  };
}
