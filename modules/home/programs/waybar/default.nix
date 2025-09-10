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
  style = import ./style.nix;
in {
  options.${namespace}.programs.waybar = {
    enable = mkBoolOpt false "${namespace}.programs.waybar.enable";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      waybar
      brightnessctl
      playerctl
      pamixer
      libnotify
      jq
      hyprsunset
    ];

    programs.waybar = {
      enable = true;
      style = style;
      systemd.enable = true;
      settings = {
        mainBar = mainBar;
      };
    };
  };
}
