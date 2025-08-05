{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.themes.xp;
in {
  options.${namespace}.themes.xp = with types; {
    enable = mkBoolOpt false "Enable full Windows XP Luna theme (rozniak/xfce-winxp-tc).";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      (pkgs.stdenv.mkDerivation {
        pname = "xfce-winxp-tc";
        version = "1.0";
        src = pkgs.fetchFromGitHub {
          owner = "rozniak";
          repo = "xfce-winxp-tc";
          rev = "master";
          sha256 = "0afayc1ykrnvyxbkfkpv7zmar8igmpi78pshb7xzz5xbqk6kcval";
        };

        buildPhase = "true";

        installPhase = ''
          # Themes
          mkdir -p $out/share/themes
          cp -r themes/* $out/share/themes/ || true

          # Icons
          mkdir -p $out/share/icons
          cp -r icons/* $out/share/icons/ || true

          # Cursors
          mkdir -p $out/share/icons
          cp -r cursors/* $out/share/icons/ || true

          # Sounds
          mkdir -p $out/share/sounds
          cp -r sounds/* $out/share/sounds/ || true

          # Shell (Panel-Layout, Startbutton etc.)
          mkdir -p $out/share/xfce-winxp/shell
          cp -r shell/* $out/share/xfce-winxp/shell/ || true

          # Base (Bootloader, Greeter)
          mkdir -p $out/share/xfce-winxp/base
          cp -r base/* $out/share/xfce-winxp/base/ || true

          # Wallpapers (Bliss etc.)
          mkdir -p $out/share/backgrounds/xfce-winxp
          cp -r wallpapers/* $out/share/backgrounds/xfce-winxp/ || true

          # Windows (Extra-Ressourcen)
          mkdir -p $out/share/xfce-winxp/windows
          cp -r windows/* $out/share/xfce-winxp/windows/ || true

          # Fonts (optional – Tahoma ist nicht FOSS, aber evtl. enthalten)
          mkdir -p $out/share/fonts
          cp -r fonts/* $out/share/fonts/ || true
        '';

        dontCheckForBrokenSymlinks = true;
      })
    ];

    environment.pathsToLink = [
      "/share/themes"
      "/share/icons"
      "/share/sounds"
      "/share/fonts"
      "/share/backgrounds"
      "/share/xfce-winxp"
    ];
  };
}
