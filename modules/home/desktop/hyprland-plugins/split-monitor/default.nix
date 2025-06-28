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
  cfg = config.${namespace}.desktop.hyprland.plugins.split-monitor;

  splitPlugin = pkgs.stdenv.mkDerivation {
    pname = "split-monitor-workspaces";
    version = "git";

    src = inputs.split-monitor-workspaces;

    nativeBuildInputs = [pkgs.cmake];
    buildInputs = [inputs.hyprland];

    cmakeFlags = [
      "-DHyprland_DIR=${inputs.hyprland}/lib/cmake/Hyprland"
    ];

    installPhase = ''
      mkdir -p $out/lib
      cp libsplit-monitor-workspaces.so $out/lib/
    '';
  };
in {
  options.${namespace}.desktop.hyprland.plugins.split-monitor = with types; {
    enable = mkBoolOpt false "Enable the split-monitor-workspaces plugin for Hyprland.";
  };

  config = mkIf cfg.enable {
    programs.hyprland.plugins = [splitPlugin];
  };
}
