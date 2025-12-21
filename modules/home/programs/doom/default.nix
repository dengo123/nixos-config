# modules/home/programs/doom/default.nix
{
  inputs,
  pkgs,
  lib,
  config,
  namespace,
  ...
}:
with lib;
with lib.${namespace};
let
  cfg = config.${namespace}.programs.doom;
in
{
  options.${namespace}.programs.doom = with types; {
    enable = mkBoolOpt false "Enable Doom Emacs via Unstraightened.";

    doomDir =
      mkOpt types.path ./dot-doom
        "Pfad zu deiner Doom-Konfiguration (init.el, config.el, packages.el).";

    emacs = mkOpt types.package pkgs.emacs "Emacs-Paket (z. B. pkgs.emacs oder pkgs.emacs-pgtk).";
  };

  imports = [ inputs.nix-doom-emacs-unstraightened.homeModule ];

  config = mkIf cfg.enable {
    programs."doom-emacs" = {
      enable = true;
      doomDir = cfg.doomDir;
      doomLocalDir = mkDefault "${config.xdg.dataHome}/doom";
      provideEmacs = true;
      emacs = cfg.emacs; # <- direkt das Package aus der Option
    };

    services.emacs = {
      enable = true;
    };

    fonts.fontconfig.enable = true;

    home.packages = with pkgs; [
      enchant
      hunspell
      hunspellDicts.de_DE
      hunspellDicts.en_US
    ];
  };
}
