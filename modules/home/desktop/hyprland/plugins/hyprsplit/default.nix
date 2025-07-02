{
  lib,
  config,
  pkgs,
  inputs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.desktop.hyprland.plugins.hyprsplit;
in {
  options.${namespace}.desktop.hyprland.plugins.hyprsplit = with types; {
    enable = mkBoolOpt false "Enable the hyprsplit plugin for Hyprland";
  };

  config = mkIf cfg.enable {
    home.packages = [
      inputs.hyprsplit.packages.${pkgs.system}.default
    ];

    # Automatically load the plugin when Hyprland starts
    wayland.windowManager.hyprland.settings.exec-once = mkAfter [
      "hyprctl plugin load ${inputs.hyprsplit.packages.${pkgs.system}.default}/lib/libhyprsplit.so"
    ];
  };
}
