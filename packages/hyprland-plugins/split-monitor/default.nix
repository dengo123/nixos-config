{
  pkgs,
  inputs,
  system,
  ...
}: let
  hyprland = inputs.hyprland.packages.${system}.default;
in
  pkgs.stdenv.mkDerivation {
    pname = "split-monitor-workspaces";
    version = "0.1";

    src = inputs.split-monitor-workspaces;

    nativeBuildInputs = [pkgs.cmake];
    buildInputs = [hyprland];

    installPhase = ''
      mkdir -p $out/lib
      find . -name '*.so' -exec cp {} $out/lib/ \;
    '';

    meta = {
      description = "Hyprland plugin to simulate split-monitor workspace behavior";
      license = pkgs.lib.licenses.gpl3Plus;
      platforms = ["x86_64-linux"];
    };
  }
