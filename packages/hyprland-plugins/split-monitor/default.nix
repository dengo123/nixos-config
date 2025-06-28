{
  lib,
  stdenv,
  cmake,
  hyprland,
  split-monitor-workspaces,
}:
stdenv.mkDerivation {
  pname = "split-monitor-workspaces";
  version = "git";

  src = split-monitor-workspaces;

  nativeBuildInputs = [cmake];
  buildInputs = [hyprland];

  cmakeFlags = [
    "-DHyprland_DIR=${hyprland}/lib/cmake/Hyprland"
  ];

  installPhase = ''
    mkdir -p $out/lib
    cp libsplit-monitor-workspaces.so $out/lib/
  '';
}
