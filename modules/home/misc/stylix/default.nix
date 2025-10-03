# modules/home/misc/stylix/default.nix
{
  config,
  lib,
  pkgs,
  inputs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.misc.stylix;
in {
  options.${namespace}.misc.stylix = with types; {
    enable = mkBoolOpt false "Enable Stylix theming.";
  };

  # WICHTIG: Immer importieren (keine config-Referenzen in imports!)
  imports = [inputs.stylix.homeModules.stylix];

  config = mkMerge [
    # 1) Hard-Off: Wenn unser Flag AUS ist, Stylix wirklich komplett abschalten
    (mkIf (!cfg.enable) {
      stylix.enable = lib.mkForce false;
      stylix.autoEnable = lib.mkForce false;
      # optional defensiv: nichts anfassen
      stylix.targets.gtk.enable = lib.mkForce false;
      stylix.iconTheme.enable = lib.mkForce false;
    })

    # 2) Unser gewünschtes Stylix-Setup, nur wenn Flag AN ist
    (mkIf cfg.enable {
      stylix = {
        enable = true;
        autoEnable = true;

        base16Scheme = import ./base16/catppuccin-mocha.nix;
        image = ./wallpapers/nixos_waves.png;

        cursor = {
          package = pkgs.bibata-cursors;
          name = "Bibata-Original-Ice";
          size = 24;
        };

        iconTheme = {
          enable = true;
          package = pkgs.papirus-icon-theme;
          light = "Papirus-Light";
          dark = "Papirus-Dark";
        };
        polarity = "light";

        targets = {
          gtk.enable = false;
          kitty.enable = false;
          waybar.enable = false;
          hyprlock.enable = false;
          neovim.enable = false;
        };
      };
      # Libadwaita hell:
      dconf.settings."org/gnome/desktop/interface".color-scheme = "default";
    })
  ];
}
