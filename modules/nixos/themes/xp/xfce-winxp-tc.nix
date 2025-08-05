{pkgs}:
pkgs.stdenv.mkDerivation {
  pname = "xfce-winxp-tc";
  version = "1.0";

  src = pkgs.fetchFromGitHub {
    owner = "rozniak";
    repo = "xfce-winxp-tc";
    rev = "master";
    sha256 = "0afayc1ykrnvyxbkfkpv7zmar8igmpi78pshb7xzz5xbqk6kcval";
  };

  nativeBuildInputs = [
    pkgs.sassc
  ];

  buildPhase = ''
    # Build GTK3 theme from SCSS
    mkdir -p build-gtk
    cd themes/adwaita-refactored
    sassc main.scss ../../build-gtk/gtk.css
    cd ../../
  '';

  installPhase = ''
        # Install GTK theme
        mkdir -p $out/share/themes/XP-Luna-GTK/gtk-3.0
        cp build-gtk/gtk.css $out/share/themes/XP-Luna-GTK/gtk-3.0/
        cat > $out/share/themes/XP-Luna-GTK/index.theme <<EOF
    [Desktop Entry]
    Type=X-GTK-Theme
    Name=XP Luna GTK
    Comment=Windows XP Luna GTK Theme (compiled from SCSS)
    EOF

        # Install XFWM themes
        mkdir -p $out/share/themes
        cp -r themes/luna/blue $out/share/themes/XP-Luna-Blue
        cp -r themes/luna/homestead $out/share/themes/XP-Luna-Homestead
        cp -r themes/luna/metallic $out/share/themes/XP-Luna-Metallic

        # Add index.theme to XFWM themes
        for dir in $out/share/themes/XP-Luna-*; do
          cat > $dir/index.theme <<EOF
    [Desktop Entry]
    Type=X-XFWM4-Theme
    Name=$(basename $dir)
    Comment=Windows XP Luna XFWM4 Theme
    EOF
        done

        # Icons
        mkdir -p $out/share/icons
        cp -r icons/* $out/share/icons/

        # Cursors
        cp -r cursors/* $out/share/icons/

        # Sounds
        mkdir -p $out/share/sounds
        cp -r sounds/* $out/share/sounds/

        # Wallpapers
        mkdir -p $out/share/backgrounds/xfce-winxp
        cp -r wallpapers/* $out/share/backgrounds/xfce-winxp/

        # Shell (Panel Assets)
        mkdir -p $out/share/xfce-winxp/shell
        cp -r shell/* $out/share/xfce-winxp/shell/

        # Base (Bootloader/Plymouth/Greeter)
        mkdir -p $out/share/xfce-winxp/base
        cp -r base/* $out/share/xfce-winxp/base/

        # Fonts (optional, falls enthalten)
        if [ -d fonts ]; then
          mkdir -p $out/share/fonts
          cp -r fonts/* $out/share/fonts/
        fi
  '';

  dontCheckForBrokenSymlinks = true;
}
