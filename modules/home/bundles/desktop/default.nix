# modules/home/bundles/desktop/default.nix
{
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
    enable = mkBoolOpt false "Whether or not to enable desktop awesome bundle configuration.";
  };

  config = mkIf cfg.enable {
    nixforge = {
      bundles = {
        files = mkDefault enabled;
        browser = mkDefault enabled;
        terminal = mkDefault enabled;
      };
      misc = {
        gtk = mkDefault enabled;
        scripts = mkDefault enabled;
        xdg = mkDefault enabled;
      };

      config = {
        systray = mkDefault enabled;
      };

      services = {
        polkit-agent = mkDefault enabled;
        xscreensaver = mkDefault enabled;
        xidlehook = mkDefault enabled;
        picom = mkDefault enabled;
        redshift = mkDefault enabled;
        udiskie = mkDefault enabled;
      };

      programs = {
        autorandr = mkDefault enabled;
      };
    };

    home.packages = with pkgs; [
      pavucontrol
      copyq
      gnome-calendar
      celluloid
      loupe
      vlc
      bitwarden-desktop
      gowall
      imagemagick
    ];
  };
}
