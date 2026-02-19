# modules/home/bundles/shell/default.nix
{
  config,
  pkgs,
  lib,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.bundles.shell;
in {
  options.${namespace}.bundles.shell = with types; {
    enable = mkBoolOpt true "Enable shell bundle.";
    mode =
      mkOpt (types.nullOr
        (types.enum [
          "full"
          "emacs"
        ]))
      null
      "Select shell bundle mode: 'full' (alles), 'emacs' (nur für vterm nötig) oder 'lite' (nur zsh + Zusatzpakete).";
  };

  config = mkIf cfg.enable (mkMerge [
    # Shared extra packages in all modes
    {
      home.packages = with pkgs; [
        coreutils
        killall
        tldr
        wget
        ripgrep
        fastfetch
        peaclock
      ];
    }

    # FULL
    (mkIf (cfg.mode == "full") {
      ${namespace}.programs = {
        atuin = enabled;
        btop = enabled;
        eza = enabled;
        fzf = enabled;
        starship = enabled;
        tmux = enabled;
        yazi = enabled;
        zoxide = enabled;
        zsh = enabled;
        lazygit = enabled;
      };
    })

    # EMACS: only for vterm necessary
    (mkIf (cfg.mode == "emacs") {
      ${namespace}.programs = {
        atuin = enabled;
        btop = enabled;
        eza = enabled;
        starship = enabled;
        zsh = enabled;
        tmux = enabled;
      };
    })

    # NULL: only zsh
    (mkIf (cfg.mode == null) {
      ${namespace}.programs = {
        zsh = enabled;
      };
    })
  ]);
}
