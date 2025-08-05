{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.themes.xp.fonts;
in {
  options.${namespace}.themes.xp.fonts = {
    enable = mkBoolOpt true "Enable font configuration";
    packages = mkOpt (types.listOf types.package) [] "Extra font packages to install system-wide";
    fontconfig.enable = mkBoolOpt true "Enable fontconfig and default settings";
  };

  config = mkIf cfg.enable {
    fonts = {
      packages =
        cfg.packages
        ++ [
          # Standard Unicode und Emoji
          pkgs.noto-fonts
          pkgs.noto-fonts-cjk-sans
          pkgs.noto-fonts-emoji

          # XP-ähnliche Open-Source-Fonts
          pkgs.dejavu_fonts
          pkgs.liberation_ttf
          pkgs.cantarell-fonts

          # Optional Nerd Fonts für Symbole (DWM/Bar)
          pkgs.nerd-fonts.symbols-only
        ];

      fontconfig = mkIf cfg.fontconfig.enable {
        antialias = true;
        hinting.enable = true;
        hinting.autohint = false;
        subpixel.rgba = "rgb";

        # XP-ähnliche Standardfonts
        defaultFonts = {
          monospace = [
            "Liberation Mono"
            "DejaVu Sans Mono"
          ];
          sansSerif = [
            "DejaVu Sans"
            "Liberation Sans"
            "Cantarell"
          ];
          serif = [
            "Liberation Serif"
            "DejaVu Serif"
          ];
        };
      };
    };
  };
}
