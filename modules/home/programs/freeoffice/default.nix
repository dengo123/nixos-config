# modules/home/programs/freeoffice/default.nix
{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace};
let
  cfg = config.${namespace}.programs.freeoffice;
in
{
  options.${namespace}.programs.freeoffice = with types; {
    enable = mkBoolOpt false "Enable SoftMaker FreeOffice and helpers.";

    createShortcuts = mkBoolOpt true ''
      Install short wrapper commands:
      - textmaker
      - planmaker
      - presentations
      which call the corresponding freeoffice-* binaries.
    '';

    createDesktopEntries = mkBoolOpt true ''
      Create custom .desktop entries using the short names
      (TextMaker, PlanMaker, Presentations) under ~/.local/share/applications.
    '';
  };

  config = mkIf cfg.enable {
    # Basis-Paket
    home.packages = [
      pkgs.freeoffice

      hunspell
      hunspellDicts.de_DE
      hunspellDicts.en_US
    ]
    ++ (optional cfg.createShortcuts (
      pkgs.writeShellScriptBin "textmaker" ''
        exec freeoffice-textmaker "$@"
      ''
    ))
    ++ (optional cfg.createShortcuts (
      pkgs.writeShellScriptBin "planmaker" ''
        exec freeoffice-planmaker "$@"
      ''
    ))
    ++ (optional cfg.createShortcuts (
      pkgs.writeShellScriptBin "presentations" ''
        exec freeoffice-presentations "$@"
      ''
    ));

    # Eigene .desktop Files, die die kurzen Wrapper nutzen
    xdg.desktopEntries = mkIf cfg.createDesktopEntries {
      textmaker = {
        name = "TextMaker";
        comment = "SoftMaker FreeOffice TextMaker";
        exec = "textmaker %U";
        terminal = false;
        categories = [
          "Office"
          "WordProcessor"
        ];
      };

      planmaker = {
        name = "PlanMaker";
        comment = "SoftMaker FreeOffice PlanMaker";
        exec = "planmaker %U";
        terminal = false;
        categories = [
          "Office"
          "Spreadsheet"
        ];
      };

      presentations = {
        name = "Presentations";
        comment = "SoftMaker FreeOffice Presentations";
        exec = "presentations %U";
        terminal = false;
        categories = [
          "Office"
          "Presentation"
        ];
      };
    };
  };
}
