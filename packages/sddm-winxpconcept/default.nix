# packages/sddm-winxpconcept/default.nix
{
  stdenvNoCC,
  fetchFromGitHub,
}:
stdenvNoCC.mkDerivation {
  pname = "sddm-theme-winxpconcept";
  version = "2e93ea0-qt6-2";

  src = fetchFromGitHub {
    owner = "yeyushengfan258";
    repo = "WinXPconcept-kde";
    rev = "2e93ea08052acc19b4177f5b4f1c266dfe673a1f";
    hash = "sha256-HAiEHlsT4BmK/yLSDIpZV2gheI2vcdyaLRemxZOGAeI=";
  };

  postPatch = ''
    set -euo pipefail
    shopt -s globstar nullglob

    for f in sddm/WinXPconcept/**/*.qml; do
      # Controls 1.x -> Controls 2 (Qt6)
      substituteInPlace "$f" \
        --replace "import QtQuick.Controls 1.0" "import QtQuick.Controls" \
        --replace "import QtQuick.Controls 1.1" "import QtQuick.Controls" \
        --replace "import QtQuick.Controls 1.2" "import QtQuick.Controls" \
        --replace "import QtQuick.Controls 1.3" "import QtQuick.Controls" \
        --replace "import QtQuick.Controls 1.4" "import QtQuick.Controls" \
        --replace "import QtQuick.Controls 1.5" "import QtQuick.Controls" || true

      # GraphicalEffects Qt5 -> Qt6 compat
      substituteInPlace "$f" \
        --replace "import QtGraphicalEffects 1.0" "import Qt5Compat.GraphicalEffects" \
        --replace "import QtGraphicalEffects 1.1" "import Qt5Compat.GraphicalEffects" \
        --replace "import QtGraphicalEffects" "import Qt5Compat.GraphicalEffects" || true
    done
  '';

  installPhase = ''
        runHook preInstall

        themeDir="$out/share/sddm/themes/WinXPconcept"
        mkdir -p "$(dirname "$themeDir")"
        cp -r sddm/WinXPconcept "$themeDir"

        # Qt6 Greeter erzwingen
        cat > "$themeDir/metadata.desktop" <<'EOF'
    [Desktop Entry]
    Name=WinXPconcept
    Type=sddm-theme
    QtVersion=6
    EOF

        runHook postInstall
  '';
}
