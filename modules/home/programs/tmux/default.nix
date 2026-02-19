# modules/home/programs/tmux/default.nix
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

      # tmux UX
      mouse = true;
      keyMode = "vi";
      baseIndex = 1;
      historyLimit = 100000;

      # zsh
      shell = "${pkgs.zsh}/bin/zsh";

      # safer default; works in vterm + most terminals
      terminal = "screen-256color";

      # Doom-ish: ergonomic leader, avoids vterm conflicts better than C-s
      prefix = "C-a";

      plugins = with pkgs.tmuxPlugins; [
        sensible
        vim-tmux-navigator
        resurrect
        continuum
      ];

      extraConfig = ''
        ##### Prefix + ergonomics #####
        # Make C-a behave well (and still allow sending literal C-a)
        bind C-a send-prefix

        # Faster escape for vi-mode (optional)
        set -sg escape-time 0

        ##### Clipboard #####
        set -g set-clipboard on
        # If you want explicit copy-to-system-clipboard in copy-mode:
        # (works well on Wayland with wl-clipboard, on X11 with xclip)
        # You can install wl-clipboard/xclip via your HM/NixOS packages.
        # We'll keep it generic and just enable tmux clipboard integration.

        ##### Status / keys #####
        set -g status-keys vi
        set -g mode-keys vi

        ##### Window/Pane indexing #####
        set -g base-index 1
        setw -g pane-base-index 1
        set -g renumber-windows on

        ##### Doom-like pane navigation #####
        # Move between panes with h/j/k/l (like Evil window nav)
        bind h select-pane -L
        bind j select-pane -D
        bind k select-pane -U
        bind l select-pane -R

        # Resize panes with H/J/K/L
        bind -r H resize-pane -L 5
        bind -r J resize-pane -D 3
        bind -r K resize-pane -U 3
        bind -r L resize-pane -R 5

        ##### Doom-like splits #####
        # 'v' = vertical split (left/right), 's' = horizontal split (top/bottom)
        bind v split-window -h -c "#{pane_current_path}"
        bind s split-window -v -c "#{pane_current_path}"

        # New window opens in current path
        bind c new-window -c "#{pane_current_path}"

        ##### Quick actions #####
        bind q kill-pane
        bind Q kill-window
        bind = select-layout even-horizontal
        bind + select-layout even-vertical
        bind \\ select-layout tiled
        bind m select-layout main-vertical

        ##### Copy-mode / selection (vi) #####
        # Start selection with v, copy with y (like vim)
        bind -T copy-mode-vi v send -X begin-selection
        bind -T copy-mode-vi y send -X copy-selection-and-cancel
        bind -T copy-mode-vi Escape send -X cancel

        ##### Plugins #####
        # Continuum/resurrect
        set -g @continuum-restore 'on'
        set -g @resurrect-capture-pane-contents 'on'
        # Save/restore shortcuts:
        # prefix + C-s (save) / prefix + C-r (restore) are common,
        # but C-s conflicts in some terminals; use prefix + S / R instead:
        bind S run-shell '~/.tmux/plugins/tmux-resurrect/scripts/save.sh'
        bind R run-shell '~/.tmux/plugins/tmux-resurrect/scripts/restore.sh'
      '';
    };
  };
}
