# modules/home/misc/gtk/default.nix
{
  config,
  lib,
  pkgs,
  namespace,
  osConfig ? null,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.misc.gtk;

  systemFonts =
    if osConfig != null
    then osConfig.${namespace}.system.fonts
    else null;

  sansFont =
    if
      systemFonts
      != null
      && systemFonts.fontconfig.enable
      && systemFonts.fontconfig.defaults.sansSerif != []
    then builtins.head systemFonts.fontconfig.defaults.sansSerif
    else "Inter";

  defaultFontSize =
    if systemFonts != null
    then systemFonts.gtk.fontSize
    else 11;

  systemCursor =
    if osConfig != null
    then osConfig.${namespace}.system.cursor
    else null;

  defaultCursorPackage =
    if systemCursor != null && systemCursor.enable
    then systemCursor.package
    else pkgs.bibata-cursors;

  defaultCursorName =
    if systemCursor != null && systemCursor.enable
    then systemCursor.name
    else "Bibata-Original-Ice";

  defaultCursorSize =
    if systemCursor != null && systemCursor.enable
    then systemCursor.size
    else 24;

  iconPkg = pkgs.papirus-icon-theme;

  gtk3Theme = "adw-gtk3";
  gtk4Theme = "Adwaita";
in {
  options.${namespace}.misc.gtk = with types; {
    enable = mkBoolOpt false "Enable GTK base theming.";

    iconTheme = mkOpt (types.enum [
      "Papirus-Light"
      "Papirus-Dark"
    ]) "Papirus-Dark" "Choose GTK icon theme.";

    fontSize = mkOpt types.int defaultFontSize "GTK application font size.";

    cursor = {
      package = mkOpt types.package defaultCursorPackage "Cursor package.";
      name = mkOpt types.str defaultCursorName "Cursor theme name.";
      size = mkOpt types.int defaultCursorSize "Cursor size.";
    };
  };

  config = mkIf cfg.enable {
    home.packages = [
      pkgs.adw-gtk3
      iconPkg
      cfg.cursor.package
    ];

    home.pointerCursor = {
      x11.enable = true;
      package = cfg.cursor.package;
      name = cfg.cursor.name;
      size = cfg.cursor.size;
    };

    xdg.configFile."gtk-3.0/settings.ini".text = ''
      [Settings]
      gtk-theme-name=${gtk3Theme}
      gtk-icon-theme-name=${cfg.iconTheme}
      gtk-cursor-theme-name=${cfg.cursor.name}
      gtk-cursor-theme-size=${toString cfg.cursor.size}
      gtk-font-name=${sansFont} ${toString cfg.fontSize}
      gtk-application-prefer-dark-theme=0
    '';

    xdg.configFile."gtk-4.0/settings.ini".text = ''
      [Settings]
      gtk-theme-name=${gtk4Theme}
      gtk-icon-theme-name=${cfg.iconTheme}
      gtk-cursor-theme-name=${cfg.cursor.name}
      gtk-cursor-theme-size=${toString cfg.cursor.size}
      gtk-font-name=${sansFont} ${toString cfg.fontSize}
      gtk-application-prefer-dark-theme=0
    '';

    ${namespace}.misc.scripts.apply-gtk-theme = mkDefault enabled;
  };
}
