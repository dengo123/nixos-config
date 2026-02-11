# modules/home/programs/doom/default.nix
{
  inputs,
  pkgs,
  lib,
  config,
  namespace,
  osConfig ? null,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.programs.doom;

  x11Enabled =
    osConfig
    != null
    && ((osConfig.services.xserver.enable or false) == true);

  emacsPkg =
    if x11Enabled
    then pkgs.emacs
    else pkgs.emacs-pgtk;
in {
  options.${namespace}.programs.doom = with types; {
    enable = mkBoolOpt false "Enable Doom Emacs via Unstraightened.";

    doomDir =
      mkOpt types.path ./dot-doom
      "Path to my doom config (init.el, config.el, packages.el).";
  };

  imports = [inputs.nix-doom-emacs-unstraightened.homeModule];

  config = mkIf cfg.enable {
    programs."doom-emacs" = {
      enable = true;
      doomDir = cfg.doomDir;
      doomLocalDir = mkDefault "${config.xdg.dataHome}/doom";
      provideEmacs = true;
      emacs = emacsPkg;
    };

    services.emacs = {
      enable = true;
      startWithUserSession = "graphical";
    };

    fonts.fontconfig.enable = true;

    home.packages = with pkgs; [
      enchant
      hunspell
      hunspellDicts.de_DE
      hunspellDicts.en_US
    ];

    home.sessionVariables.EDITOR = "emacsclient -t";
    home.sessionVariables.VISUAL = "emacsclient -c -a";
  };
}
