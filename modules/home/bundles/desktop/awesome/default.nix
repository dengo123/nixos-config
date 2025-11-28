{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace};
let
  cfg = config.${namespace}.bundles.desktop.awesome;
in
{
  options.${namespace}.bundles.desktop.awesome = with types; {
    enable = mkBoolOpt false "Whether or not to enable desktop awesome bundle configuration.";
  };

  config = mkIf cfg.enable {
    nixforge = {
      desktop = {
        awesome = enabled;
      };
      config = {
        systray = enabled;
      };
      misc = {
        gtk = {
          enable = true;
          iconTheme = "Papirus-Dark"; # or "Adwaita" | "Papirus-Dark"
        };
      };
      services = {
        autorandr = enabled;
        xscreensaver = {
          enable = true;
          suspend = {
            enable = true;
            secondsToSuspend = 1500;
          };
        };
        redshift = {
          enable = true;
          provider = "manual";
          latitude = 50.1;
          longitude = 8.6;
        };
      };
      programs = {
        nemo = {
          enable = true;
          withBundle = false;
        };
      };
    };

    home.packages = with pkgs; [
      pavucontrol
    ];
  };
}
