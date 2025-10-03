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
    enable = mkBoolOpt false "Whether or not to enable desktop hyprland bundle configuration.";
  };

  config = mkIf cfg.enable {
    nixforge = {
      desktop = {
        awesome = enabled;
      };
      config = {
        systray = enabled;
      };
      misc.gtk = enabled;
      services = {
        autorandr = enabled;
      };
      programs = {
        nemo = {
          enable = true;
          withBundle = false;
        };
      };
    };

    home.packages = with pkgs; [
      adwaita-icon-theme
      gnome-system-monitor
      morewaita-icon-theme
      qogir-icon-theme
    ];
  };
}
