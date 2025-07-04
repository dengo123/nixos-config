{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.programs.waybar;

  mainBar = import ./mainBar.nix;
  style = import ./style.nix {inherit config lib pkgs;};
in {
  options.${namespace}.programs.waybar = {
    enable = mkBoolOpt false "${namespace}.programs.waybar.enable";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      waybar
      rofi
      blueman
      bluetui
      networkmanager
      networkmanager_dmenu
      brightnessctl
      playerctl
      pamixer
      gammastep
      libnotify
      jq
      # evtl. weitere Skriptabhängigkeiten
    ];

    programs.waybar = {
      enable = true;
      style = style;
      systemd.enable = true;
      settings = {
        mainBar = mainBar;
        # später z. B. auch: secondaryBar = import ./secondaryBar.nix;
      };
    };
  };
}
