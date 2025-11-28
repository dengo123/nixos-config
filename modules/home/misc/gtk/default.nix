# modules/home/misc/gtk/default.nix
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
  cfg = config.${namespace}.misc.gtk;

  # Icon-Paket + Name
  iconPkg =
    if cfg.iconTheme == "Papirus-Light" || cfg.iconTheme == "Papirus-Dark" then
      pkgs.papirus-icon-theme
    else
      pkgs.adwaita-icon-theme;

  iconName = cfg.iconTheme; # exakt "Adwaita", "Papirus-Light" oder "Papirus-Dark"

  # GTK-Theme:
  # GTK3 nutzt adw-gtk3 (Light), GTK4 bleibt Adwaita (Light).
  gtk3Theme = "adw-gtk3";
  gtk4Theme = "Adwaita";

  # einfache helle Palette (anpassbar über Optionen)
  palette = cfg.palette;
in
{
  options.${namespace}.misc.gtk = with types; {
    enable = mkBoolOpt false "Enable GTK theming (light) with Bibata cursor and Intel One Mono.";

    iconTheme = mkOpt (types.enum [
      "Adwaita"
      "Papirus-Light"
      "Papirus-Dark"
    ]) "Adwaita" "Choose system icon theme.";

    fontName = mkOpt types.str "Intel One Mono" "GTK application font family.";
    fontSize = mkOpt types.int 12 "GTK application font size.";

    # einfache Farbpalette für gtk.css (wird auf GTK3/4 angewendet)
    palette =
      mkOpt
        (types.submodule {
          options = {
            base = mkOpt types.str "#FAFAFC" "Base window background.";
            text = mkOpt types.str "#1E1E1E" "Primary text color.";
            accent = mkOpt types.str "#2A7FFF" "Accent color (buttons, links).";
            accentText = mkOpt types.str "#FFFFFF" "Text color on accent bg.";
            header = mkOpt types.str "#E9EEF7" "Headerbar background.";
            headerText = mkOpt types.str "#1A1A1A" "Headerbar text color.";
            selection = mkOpt types.str "#CCE0FF" "Selection background.";
            selectionText = mkOpt types.str "#0B0B0B" "Selection text color.";
            border = mkOpt types.str "#C9CED6" "Thin border color.";
          };
        })
        {
          base = "#FAFAFC";
          text = "#1E1E1E";
          accent = "#2A7FFF";
          accentText = "#FFFFFF";
          header = "#E9EEF7";
          headerText = "#1A1A1A";
          selection = "#CCE0FF";
          selectionText = "#0B0B0B";
          border = "#C9CED6";
        }
        "Simple light palette used by gtk.css.";
  };

  config = mkIf cfg.enable {
    # Nutzer-Pakete (reicht für GTK/Icons/Cursor/Fonts)
    home.packages = with pkgs; [
      # Cursor + Font
      bibata-cursors
      intel-one-mono

      # GTK3 Light Theme (modernes Adwaita für GTK3)
      adw-gtk3

      # Icons
      iconPkg
    ];

    # Cursor auch für X11-Zeiger (awesome) setzen
    xsession.pointerCursor = {
      package = pkgs.bibata-cursors;
      name = "Bibata-Original-Ice";
      size = 24;
    };

    # GTK3 settings.ini
    xdg.configFile."gtk-3.0/settings.ini".text = ''
      [Settings]
      gtk-theme-name=${gtk3Theme}
      gtk-icon-theme-name=${iconName}
      gtk-cursor-theme-name=Bibata-Original-Ice
      gtk-cursor-theme-size=24
      gtk-font-name=${cfg.fontName} ${toString cfg.fontSize}
      gtk-application-prefer-dark-theme=0
    '';

    # GTK4 settings.ini
    xdg.configFile."gtk-4.0/settings.ini".text = ''
      [Settings]
      gtk-theme-name=${gtk4Theme}
      gtk-icon-theme-name=${iconName}
      gtk-cursor-theme-name=Bibata-Original-Ice
      gtk-cursor-theme-size=24
      gtk-font-name=${cfg.fontName} ${toString cfg.fontSize}
      gtk-application-prefer-dark-theme=0
    '';

    # GTK3 CSS – einfache Palette/Overrides
    xdg.configFile."gtk-3.0/gtk.css".text = ''
      @define-color theme_bg_color        ${palette.base};
      @define-color theme_fg_color        ${palette.text};
      @define-color accent_color          ${palette.accent};
      @define-color accent_fg_color       ${palette.accentText};
      @define-color headerbar_bg_color    ${palette.header};
      @define-color headerbar_fg_color    ${palette.headerText};
      @define-color borders               ${palette.border};
      @define-color selected_bg_color     ${palette.selection};
      @define-color selected_fg_color     ${palette.selectionText};

      window, dialog, .background { background-color: @theme_bg_color; color: @theme_fg_color; }
      headerbar, .titlebar {
        background-color: @headerbar_bg_color;
        color: @headerbar_fg_color;
        border-bottom: 1px solid @borders;
      }
      button, .button {
        border-color: @borders;
      }
      button.suggested-action, .suggested-action {
        background-color: @accent_color;
        color: @accent_fg_color;
      }
      selection, *:selected {
        background-color: @selected_bg_color;
        color: @selected_fg_color;
      }
      menubar, menu, .menu, .popover {
        background-color: @theme_bg_color;
        color: @theme_fg_color;
        border-color: @borders;
      }

      /* >>> NEU: Tooltips lesbar machen <<< */
      tooltip,
      .tooltip,
      #gtk-tooltip-window {
        background-color: @headerbar_bg_color;
        color: @theme_fg_color;
        border: 1px solid @borders;
      }
      tooltip * {
        color: @theme_fg_color;
      }
    '';

    xdg.configFile."gtk-4.0/gtk.css".text = ''
      @define-color theme_bg_color        ${palette.base};
      @define-color theme_fg_color        ${palette.text};
      @define-color accent_color          ${palette.accent};
      @define-color accent_fg_color       ${palette.accentText};
      @define-color headerbar_bg_color    ${palette.header};
      @define-color headerbar_fg_color    ${palette.headerText};
      @define-color borders               ${palette.border};
      @define-color selected_bg_color     ${palette.selection};
      @define-color selected_fg_color     ${palette.selectionText};

      window, dialog, .background { background-color: @theme_bg_color; color: @theme_fg_color; }
      headerbar, .titlebar {
        background-color: @headerbar_bg_color;
        color: @headerbar_fg_color;
        border-bottom: 1px solid @borders;
      }
      button.suggested-action {
        background-color: @accent_color;
        color: @accent_fg_color;
      }
      selection, text selection, label selection {
        background-color: @selected_bg_color;
        color: @selected_fg_color;
      }
      popover, menu, menubar {
        background-color: @theme_bg_color;
        color: @theme_fg_color;
        border: 1px solid @borders;
      }

      /* >>> NEU: Tooltips für GTK4 <<< */
      tooltip,
      .tooltip {
        background-color: @headerbar_bg_color;
        color: @theme_fg_color;
        border: 1px solid @borders;
      }
      tooltip * {
        color: @theme_fg_color;
      }

      :root { color-scheme: only light; }
    '';
  };
}
