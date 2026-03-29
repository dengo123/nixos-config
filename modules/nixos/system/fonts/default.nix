# modules/nixos/system/fonts/default.nix
{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.system.fonts;
in {
  options.${namespace}.system.fonts = with types; {
    enable = mkBoolOpt false "Enable system font configuration.";

    packages = {
      enableDefaults = mkBoolOpt true "Install the default system font set.";
      extra = mkOpt (listOf package) [] "Extra font packages to install system-wide.";
    };

    fontconfig = {
      enable = mkBoolOpt true "Enable fontconfig and default font settings.";

      antialias = mkBoolOpt true "Enable antialiasing.";

      hinting = {
        enable = mkBoolOpt true "Enable font hinting.";
        autohint = mkBoolOpt false "Enable font autohinting.";
      };

      subpixel = mkOpt (enum ["none" "rgb" "bgr" "vrgb" "vbgr"]) "rgb" "Subpixel order.";

      defaults = {
        sansSerif = mkOpt (listOf str) ["Inter"] "Default sans-serif font families.";
        serif = mkOpt (listOf str) ["Noto Serif"] "Default serif font families.";
        monospace = mkOpt (listOf str) ["Intel One Mono"] "Default monospace font families.";
        emoji = mkOpt (listOf str) ["Noto Color Emoji"] "Default emoji font families.";
      };
    };

    gtk = {
      fontSize = mkOpt int 11 "Default GTK font size.";
    };
  };

  config = mkIf cfg.enable {
    fonts = {
      packages =
        optionals cfg.packages.enableDefaults [
          pkgs.noto-fonts
          pkgs.noto-fonts-cjk-sans
          pkgs.noto-fonts-color-emoji
          pkgs.noto-fonts
          pkgs.inter
          pkgs.intel-one-mono
          pkgs.nerd-fonts.fira-code
          pkgs.nerd-fonts.jetbrains-mono
        ]
        ++ cfg.packages.extra;

      fontconfig = mkIf cfg.fontconfig.enable {
        antialias = cfg.fontconfig.antialias;

        hinting = {
          enable = cfg.fontconfig.hinting.enable;
          autohint = cfg.fontconfig.hinting.autohint;
        };

        subpixel.rgba = cfg.fontconfig.subpixel;

        defaultFonts = {
          sansSerif = cfg.fontconfig.defaults.sansSerif;
          serif = cfg.fontconfig.defaults.serif;
          monospace = cfg.fontconfig.defaults.monospace;
          emoji = cfg.fontconfig.defaults.emoji;
        };
      };
    };
  };
}
