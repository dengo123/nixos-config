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
      services = {
        autorandr = enabled;
      };
    };

    home.packages = with pkgs; [
      adwaita-icon-theme
      gnome-system-monitor
      gnome-control-center
      morewaita-icon-theme
      qogir-icon-theme
      nemo
    ];
  };
}
