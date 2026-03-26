# modules/home/misc/scripts/apply-gtk-theme/default.nix
{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.misc.scripts.apply-gtk-theme;

  applyGtkTheme = pkgs.writeShellScriptBin "apply-gtk-theme" ''
        #!/usr/bin/env bash
        set -euo pipefail

        export HOME="''${HOME:-${config.home.homeDirectory}}"
        export XDG_CACHE_HOME="''${XDG_CACHE_HOME:-$HOME/.cache}"
        export XDG_CONFIG_HOME="''${XDG_CONFIG_HOME:-$HOME/.config}"

        export STATE_FILE="$XDG_CACHE_HOME/awesome/theme-state.json"
        export GTK3_DIR="$XDG_CONFIG_HOME/gtk-3.0"
        export GTK4_DIR="$XDG_CONFIG_HOME/gtk-4.0"
        export GTK3_CSS="$GTK3_DIR/gtk.css"
        export GTK4_CSS="$GTK4_DIR/gtk.css"

        mkdir -p "$GTK3_DIR" "$GTK4_DIR"

        if [ ! -f "$STATE_FILE" ]; then
          echo "apply-gtk-theme: state file not found: $STATE_FILE" >&2
          exit 1
        fi

        ${pkgs.python3}/bin/python3 <<'PY'
    import json
    import os
    from pathlib import Path

    state_file = Path(os.environ["STATE_FILE"])
    gtk3_css = Path(os.environ["GTK3_CSS"])
    gtk4_css = Path(os.environ["GTK4_CSS"])

    data = json.loads(state_file.read_text())
    theme = data["theme"]
    palette = theme["palette"]
    roles = theme["roles"]

    def role_color(name: str) -> str:
        palette_key = roles[name]
        return palette[palette_key]

    colors = {
        "theme_bg_color": role_color("background"),
        "theme_fg_color": role_color("text"),
        "accent_color": role_color("primary"),
        "accent_fg_color": role_color("text_invert"),
        "headerbar_bg_color": role_color("background"),
        "headerbar_fg_color": role_color("text"),
        "borders": role_color("text_invert_focus"),
        "selected_bg_color": role_color("secondary"),
        "selected_fg_color": role_color("text_invert"),
    }

    gtk3_css_text = f"""
    @define-color theme_bg_color        {colors['theme_bg_color']};
    @define-color theme_fg_color        {colors['theme_fg_color']};
    @define-color accent_color          {colors['accent_color']};
    @define-color accent_fg_color       {colors['accent_fg_color']};
    @define-color headerbar_bg_color    {colors['headerbar_bg_color']};
    @define-color headerbar_fg_color    {colors['headerbar_fg_color']};
    @define-color borders               {colors['borders']};
    @define-color selected_bg_color     {colors['selected_bg_color']};
    @define-color selected_fg_color     {colors['selected_fg_color']};

    window, dialog, .background {{
      background-color: @theme_bg_color;
      color: @theme_fg_color;
    }}

    headerbar, .titlebar {{
      background-color: @headerbar_bg_color;
      color: @headerbar_fg_color;
      border-bottom: 1px solid @borders;
    }}

    button, .button {{
      border-color: @borders;
    }}

    button.suggested-action, .suggested-action {{
      background-color: @accent_color;
      color: @accent_fg_color;
    }}

    selection, *:selected {{
      background-color: @selected_bg_color;
      color: @selected_fg_color;
    }}

    menubar, menu, .menu, .popover {{
      background-color: @theme_bg_color;
      color: @theme_fg_color;
      border-color: @borders;
    }}

    tooltip,
    .tooltip,
    #gtk-tooltip-window {{
      background-color: @headerbar_bg_color;
      color: @theme_fg_color;
      border: 1px solid @borders;
    }}

    tooltip * {{
      color: @theme_fg_color;
    }}
    """.strip() + "\n"

    gtk4_css_text = f"""
    @define-color theme_bg_color        {colors['theme_bg_color']};
    @define-color theme_fg_color        {colors['theme_fg_color']};
    @define-color accent_color          {colors['accent_color']};
    @define-color accent_fg_color       {colors['accent_fg_color']};
    @define-color headerbar_bg_color    {colors['headerbar_bg_color']};
    @define-color headerbar_fg_color    {colors['headerbar_fg_color']};
    @define-color borders               {colors['borders']};
    @define-color selected_bg_color     {colors['selected_bg_color']};
    @define-color selected_fg_color     {colors['selected_fg_color']};

    window, dialog, .background {{
      background-color: @theme_bg_color;
      color: @theme_fg_color;
    }}

    headerbar, .titlebar {{
      background-color: @headerbar_bg_color;
      color: @headerbar_fg_color;
      border-bottom: 1px solid @borders;
    }}

    button.suggested-action {{
      background-color: @accent_color;
      color: @accent_fg_color;
    }}

    selection, text selection, label selection {{
      background-color: @selected_bg_color;
      color: @selected_fg_color;
    }}

    popover, menu, menubar {{
      background-color: @theme_bg_color;
      color: @theme_fg_color;
      border: 1px solid @borders;
    }}

    tooltip,
    .tooltip {{
      background-color: @headerbar_bg_color;
      color: @theme_fg_color;
      border: 1px solid @borders;
    }}

    tooltip * {{
      color: @theme_fg_color;
    }}

    :root {{ color-scheme: only light; }}
    """.strip() + "\n"

    gtk3_css.write_text(gtk3_css_text)
    gtk4_css.write_text(gtk4_css_text)
    PY
  '';
in {
  options.${namespace}.misc.scripts.apply-gtk-theme = with types; {
    enable = mkBoolOpt false "Enable apply-gtk-theme script.";
  };

  config = mkIf cfg.enable {
    home.packages = [
      applyGtkTheme
      pkgs.python3
    ];
  };
}
