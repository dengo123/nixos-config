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
with lib.${namespace}; let
  cfg = config.${namespace}.programs.doom;
in {
  options.${namespace}.programs.doom = with types; {
    enable = mkBoolOpt false "Enable Doom Emacs with a user-managed Doom config.";

    package = mkOpt package inputs.self.packages.${pkgs.system}.doom-emacs "Doom Emacs framework package.";

    emacsPackage = mkOpt package pkgs.emacs "Emacs package to use with Doom.";

    doomDir =
      mkOpt str "${config.home.homeDirectory}/.config/doom"
      "Path to the user-managed Doom config directory.";

    doomLocalDir =
      mkOpt str "${config.xdg.dataHome}/doom"
      "Path to Doom local data directory.";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      cfg.package
      cfg.emacsPackage
      (pkgs.texlive.combine {
        inherit
          (pkgs.texlive)
          scheme-medium
          wrapfig
          capt-of
          hyperref
          geometry
          ulem
          ;
      })
    ];

    fonts.fontconfig.enable = true;

    xdg.configFile."emacs".source = "${cfg.package}/share/doom-emacs";

    home.sessionVariables = {
      DOOMDIR = cfg.doomDir;
      DOOMLOCALDIR = cfg.doomLocalDir;
      EDITOR = "emacsclient -t";
      VISUAL = "emacsclient -c -a emacs";
    };

    services.emacs = {
      enable = true;
      startWithUserSession = "graphical";
      package = cfg.emacsPackage;
    };

    systemd.user.services.emacs.Service.Environment = [
      "DOOMDIR=${cfg.doomDir}"
      "DOOMLOCALDIR=${cfg.doomLocalDir}"
      "XDG_CONFIG_HOME=${config.xdg.configHome}"
      "XDG_DATA_HOME=${config.xdg.dataHome}"
      "XDG_CACHE_HOME=${config.xdg.cacheHome}"
      "HOME=${config.home.homeDirectory}"
    ];

    home.shellAliases = {
      doom = "${cfg.package}/bin/doom";
    };
  };
}
