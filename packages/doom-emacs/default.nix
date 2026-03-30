# packages/doom-emacs/default.nix
{
  lib,
  stdenvNoCC,
  bash,
  makeWrapper,
  fetchFromGitHub,
}:
stdenvNoCC.mkDerivation {
  pname = "doom-emacs";
  version = "unstable-2026-03-30";

  src = fetchFromGitHub {
    owner = "doomemacs";
    repo = "doomemacs";
    rev = "master";
    hash = "sha256-BSrZWdOh5ZUWzS/IBvlNc0s7V1mpHcA+q/GhxR+oXlY=";
  };

  nativeBuildInputs = [
    makeWrapper
  ];

  dontBuild = true;

  installPhase = ''
    mkdir -p $out/share/doom-emacs
    cp -R . $out/share/doom-emacs

    mkdir -p $out/bin
    makeWrapper ${bash}/bin/bash $out/bin/doom \
      --set EMACSDIR $out/share/doom-emacs \
      --add-flags $out/share/doom-emacs/bin/doom
  '';

  meta = with lib; {
    description = "Doom Emacs framework";
    homepage = "https://github.com/doomemacs/doomemacs";
    license = licenses.gpl3Only;
    platforms = platforms.linux;
    mainProgram = "doom";
  };
}
