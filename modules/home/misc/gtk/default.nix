# modules/home/misc/gtk/default.nix
{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.misc.gtk;

  iconPkg =
    if cfg.iconTheme == "Papirus-Light" || cfg.iconTheme == "Papirus-Dark"
    then pkgs.papirus-icon-theme
    else pkgs.adwaita-icon-theme;

  iconName = cfg.iconTheme;

  gtk3Theme = "adw-gtk3";
  gtk4Theme = "Adwaita";
in {
  options.${namespace}.misc.gtk = with types; {
    enable = mkBoolOpt false "Enable GTK base theming with static settings.ini and runtime-generated gtk.css.";

    iconTheme = mkOpt (types.enum [
      "Adwaita"
      "Papirus-Light"
      "Papirus-Dark"
    ]) "Adwaita" "Choose system icon theme.";

    fontName = mkOpt types.str "Intel One Mono" "GTK application font family.";
    fontSize = mkOpt types.int 12 "GTK application font size.";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      bibata-cursors
      intel-one-mono
      adw-gtk3
      iconPkg
    ];

    home.pointerCursor = {
      x11.enable = true;
      package = pkgs.bibata-cursors;
      name = "Bibata-Original-Ice";
      size = 24;
    };

    xdg.configFile."gtk-3.0/settings.ini".text = ''
      [Settings]
      gtk-theme-name=${gtk3Theme}
      gtk-icon-theme-name=${iconName}
      gtk-cursor-theme-name=Bibata-Original-Ice
      gtk-cursor-theme-size=24
      gtk-font-name=${cfg.fontName} ${toString cfg.fontSize}
      gtk-application-prefer-dark-theme=0
    '';

    xdg.configFile."gtk-4.0/settings.ini".text = ''
      [Settings]
      gtk-theme-name=${gtk4Theme}
      gtk-icon-theme-name=${iconName}
      gtk-cursor-theme-name=Bibata-Original-Ice
      gtk-cursor-theme-size=24
      gtk-font-name=${cfg.fontName} ${toString cfg.fontSize}
      gtk-application-prefer-dark-theme=0
    '';
  };
}
