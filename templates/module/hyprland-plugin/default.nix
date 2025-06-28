{
  lib,
  pkgs,
  namespace,
  ...
}: {
  name,
  src,
  hyprland,
}:
with lib;
with lib.${namespace}; let
  hyprlandPkg = hyprland.packages.${pkgs.system}.default;

  plugin = pkgs.stdenv.mkDerivation {
    inherit name src;
    version = "git";

    nativeBuildInputs = [pkgs.cmake];
    buildInputs = [hyprlandPkg];

    cmakeFlags = [
      "-DHyprland_DIR=${hyprlandPkg}/lib/cmake/Hyprland"
    ];

    installPhase = ''
      mkdir -p $out/lib
      cp lib${name}.so $out/lib/
    '';
  };

  optionPath = "${namespace}.desktop.hyprland.plugins.${name}";
in {
  options."${namespace}".desktop.hyprland.plugins.${name} = with types; {
    enable = mkBoolOpt false "Enable the ${name} plugin for Hyprland.";
  };

  config = mkIf config.${optionPath}.enable {
    wayland.windowManager.hyprland.plugins = [plugin];
  };
}
