# packages/sddm-winxpconcept/default.nix
{
  stdenvNoCC,
  fetchFromGitHub,
}:
stdenvNoCC.mkDerivation {
  pname = "sddm-theme-winxpconcept";
  version = "2e93ea0";

  src = fetchFromGitHub {
    owner = "yeyushengfan258";
    repo = "WinXPconcept-kde";
    rev = "2e93ea08052acc19b4177f5b4f1c266dfe673a1f";
    hash = "sha256-HAiEHlsT4BmK/yLSDIpZV2gheI2vcdyaLRemxZOGAeI=";
  };

  installPhase = ''
    mkdir -p $out/share/sddm/themes
    cp -r sddm/WinXPconcept $out/share/sddm/themes/WinXPconcept
  '';
}
