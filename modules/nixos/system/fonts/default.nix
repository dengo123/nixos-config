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
  options.${namespace}.system.fonts = {
    enable = mkBoolOpt true "Enable font configuration";
    packages = mkOpt (types.listOf types.package) [] "Extra font packages to install system-wide";
    fontconfig.enable = mkBoolOpt true "Enable fontconfig and default settings";
  };

  config = mkIf cfg.enable {
    fonts = {
      packages =
        cfg.packages
        ++ [
          pkgs.noto-fonts
          pkgs.noto-fonts-cjk-sans
          pkgs.noto-fonts-emoji
          pkgs.inter

          pkgs.nerd-fonts.fira-code
          pkgs.nerd-fonts.jetbrains-mono
        ];

      fontconfig = mkIf cfg.fontconfig.enable {
        antialias = true;
        hinting.enable = true;
        hinting.autohint = false;
        subpixel.rgba = "rgb";

        defaultFonts = {
          monospace = ["Intel One Mono"];
          sansSerif = ["Inter"];
          serif = ["Noto Serif"];
        };
      };
    };
  };
}
