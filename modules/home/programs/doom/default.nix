{
  inputs,
  pkgs,
  lib,
  config,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.programs.doom;
in {
  options.${namespace}.programs.doom = with types; {
    enable = mkBoolOpt false "Enable Doom Emacs via Unstraightened.";
    # Dein Doom-Config-Ordner (init.el, config.el, packages.el)
    doomDir =
      mkOpt (oneOf [
        path
        str
      ])
      ./dot-doom "Pfad zu deiner Doom-Konfiguration.";
  };

  # Upstream HM-Modul (Unstraightened) einbinden
  imports = [
    inputs.nix-doom-emacs-unstraightened.homeModule
  ];

  # Minimal: Emacs (mit Doom) aktivieren und auf deine Config zeigen
  config = mkIf cfg.enable {
    programs."doom-emacs" = {
      enable = true;
      doomDir = cfg.doomDir;
      doomLocalDir = "~/.local/share/doom"; # Upstream verlangt schreibbar
      emacs = pkgs.emacs-pgtk; # Emacs-Variante
      provideEmacs = true; # -> starte "emacs" ganz normal
      # alles andere lassen wir weg
    };

    # (optional angenehm)
    fonts.fontconfig.enable = true;
  };
}
