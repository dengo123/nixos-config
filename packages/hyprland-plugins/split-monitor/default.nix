{
  lib,
  stdenv,
  cmake,
  hyprland,
  fetchFromGitHub,
}:
stdenv.mkDerivation rec {
  pname = "split-monitor-workspaces";
  version = "0.1";

  src = fetchFromGitHub {
    owner = "Duckonaut";
    repo = "split-monitor-workspaces";
    rev = "a0015f8ba44bc194916dc797501ea06823c51926";
    sha256 = "sha256-Bj11uVqKPHI2sKsXMQdVhdMMbJ403Ps6x4jOcmVfr2Q=";
  };

  nativeBuildInputs = [cmake];
  buildInputs = [hyprland];

  cmakeFlags = [
    "-DHyprland_DIR=${hyprland}/lib/cmake/Hyprland"
  ];

  installPhase = ''
    mkdir -p $out/lib
    cp libsplit-monitor-workspaces.so $out/lib/
    patchelf --set-rpath ${hyprland}/lib:$out/lib $out/lib/libsplit-monitor-workspaces.so
  '';
}
