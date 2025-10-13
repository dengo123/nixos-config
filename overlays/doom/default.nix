# Overlay: fügt pkgs.doom-emacs-test hinzu
final: prev: {
  doom-emacs-test = prev.doomEmacs {
    # deine Config als Store-Pfad; hier relativ zu dieser Datei
    doomDir = builtins.path {
      path = ../../dotfiles/doom;
      name = "doom-config";
    };

    # schreibbares Laufzeitverzeichnis
    doomLocalDir = "~/.local/share/nix-doom";

    # Emacs-Variante: für Awesome(Xorg) -> prev.emacs, für Hyprland(Wayland) -> prev.emacs-pgtk
    emacs = prev.emacs;

    # Nix >= 2.19: robustere/effizientere Fetches
    experimentalFetchTree = true;
  };
}
