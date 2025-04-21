{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.programs.tmux;
in {
  options.${namespace}.programs.tmux = with types; {
    enable = mkBoolOpt false "Enable programs.tmux";
  };

  config = mkIf cfg.enable {
    programs.tmux = {
      enable = true;
      mouse = true;
      shell = "${pkgs.zsh}/bin/zsh";
      prefix = "C-s";
      terminal = "screen-256color"; # statt hardcoded "kitty"
      keyMode = "vi";
      baseIndex = 1;
      historyLimit = 10000;

      plugins = with pkgs; [
        tmuxPlugins.catppuccin
        tmuxPlugins.vim-tmux-navigator
        tmuxPlugins.resurrect
        tmuxPlugins.continuum
        tmuxPlugins.sensible
      ];

      extraConfig = ''
        set -g set-clipboard on
        set -g @continuum-restore on
        set -g status-keys vi
      '';
    };
  };
}
