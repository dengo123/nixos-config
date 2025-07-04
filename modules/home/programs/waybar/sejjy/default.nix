{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (lib) mkIf;
  cfg = config.programs.waybar;

  style = import ./style.nix {inherit config lib pkgs;};
  configJson = import ./config.nix {inherit config lib pkgs;};
in {
  options.programs.waybar = {
    enable = lib.mkEnableOption "Waybar status bar";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      waybar
      rofi
      blueman
      bluetui
      networkmanager
      networkmanager_dmenu # falls du auf dmenu wechseln willst
      brightnessctl
      playerctl
      pamixer
      gammastep
      libnotify
      jq
      # weitere Abhängigkeiten für deine Skripte
    ];

    programs.waybar = {
      enable = true;
      settings = configJson;
      style = style;
    };
  };
}
