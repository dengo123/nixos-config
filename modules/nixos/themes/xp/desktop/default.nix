{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.themes.xp.desktop;
in {
  options.${namespace}.themes.xp.desktop = with types; {
    enable = mkBoolOpt false "Enable XP Luna desktop theme (B00merang + Chicago95).";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      # Luna Theme (B00merang)
      (pkgs.stdenv.mkDerivation {
        pname = "luna-theme";
        version = "1.0";
        src = pkgs.fetchFromGitHub {
          owner = "B00merang-Project";
          repo = "Windows-XP";
          rev = "master";
          sha256 = "01yd84kq0nasf5qnawzmrvh4c4s1l73kvmsrx5qjfhihfsia5xg9";
        };
        buildPhase = "true";
        installPhase = ''
          mkdir -p $out/share/themes/Luna
          cp -r ./* $out/share/themes/Luna/
        '';
      })

      # Chicago95 (Icons + Cursor + Sounds)
      (pkgs.stdenv.mkDerivation {
        pname = "chicago95-extras";
        version = "1.0";
        src = pkgs.fetchFromGitHub {
          owner = "grassmunk";
          repo = "Chicago95";
          rev = "master";
          sha256 = "190n797v30iir10m6q2xdzrwqb06q8pvqr8r3m1c8cs2r6nzwibv";
        };
        buildPhase = "true";
        installPhase = ''
          # Icons
          mkdir -p $out/share/icons
          cp -r Icons/* $out/share/icons/

          # Cursors
          mkdir -p $out/share/icons
          cp -r Cursors/* $out/share/icons/

          # Sounds
          mkdir -p $out/share/sounds
          cp -r sounds/* $out/share/sounds/
        '';
      })
    ];
  };
}
