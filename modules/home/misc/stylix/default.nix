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
    enable = mkBoolOpt false "Enable stylix";
  };

  imports = [inputs.stylix.homeModules.stylix];

  config = mkIf cfg.enable {
    stylix = {
      enable = true;

      autoEnable = true;
      base16Scheme = import ./base16/catppuccin-mocha.nix;
      # base16Scheme = builtins.toString ./base16/catppuccin/colors.yaml;
      cursor = {
        package = pkgs.bibata-cursors;
        name = "Bibata-Original-Ice";
        size = 24;
      };

      fonts = {
        monospace = {
          package = pkgs.nerd-fonts.jetbrains-mono;
          name = "JetBrainsMono Nerd Font Mono";
        };
        sansSerif = {
          package = pkgs.nerd-fonts.jetbrains-mono;
          name = "JetBrainsMono Nerd Font Mono";
        };
        serif = {
          package = pkgs.nerd-fonts.jetbrains-mono;
          name = "JetBrainsMono Nerd Font Mono";
        };
        emoji = {
          package = pkgs.noto-fonts-emoji;
          name = "Noto Color Emoji";
        };
        sizes = {
          applications = 13;
          desktop = 13;
          popups = 13;
          terminal = 13;
        };
      };

      iconTheme = {
        enable = true;
        package = pkgs.papirus-icon-theme;
        light = "Papirus-Light";
        dark = "Papirus-Dark";
      };

      image = ./wallpapers/nixos_waves.png;

      polarity = "dark";
      targets = {
        kitty.enable = false;
        waybar.enable = false;
        hyprlock.enable = false;
        neovim.enable = false;
      };
    };
  };
}
