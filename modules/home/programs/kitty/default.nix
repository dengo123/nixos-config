{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.programs.kitty;
in {
  options.${namespace}.programs.kitty = with types; {
    enable = mkBoolOpt false "Enable kitty";
  };

  config = mkIf cfg.enable {
    programs.kitty = {
      enable = true;
      font = {
        name = "MesloLGS NF";
        package = pkgs.meslo-lgs-nf;
        size = 12;
      };
      themeFile = "Catppuccin-Mocha";
      shellIntegration = {
        mode = "enabled";
        enableZshIntegration = true;
      };
      keybindings = {
        "ctrl+shift+c" = "copy_to_clipboard";
        "ctrl+shift+v" = "paste_from_clipboard";
        "ctrl+shift+s" = "copy_to_selection"; # Primärauswahl (Middle-Click)
        "shift+insert" = "paste_from_selection"; # Primärauswahl einfügen

        # Tabs öffnen/schließen
        "ctrl+shift+t" = "new_tab";
        "ctrl+shift+w" = "close_tab";

        # zwischen Tabs wechseln
        "ctrl+shift+right" = "next_tab";
        "ctrl+shift+left" = "previous_tab";

        # Tabs umsortieren
        "ctrl+shift+." = "move_tab_forward";
        "ctrl+shift+," = "move_tab_backward";

        # direkt zu Tab N springen
        "ctrl+shift+1" = "goto_tab 1";
        "ctrl+shift+2" = "goto_tab 2";
        "ctrl+shift+3" = "goto_tab 3";
        "ctrl+shift+4" = "goto_tab 4";
        "ctrl+shift+5" = "goto_tab 5";
        "ctrl+shift+6" = "goto_tab 6";
        "ctrl+shift+7" = "goto_tab 7";
        "ctrl+shift+8" = "goto_tab 8";
        "ctrl+shift+9" = "goto_tab 9";
      };
      settings = {
        tab_bar_edge = "top";
        tab_bar_style = "fade";
        tab_title_template = "{index}: {active_process_name}";
        confirm_os_window_close = "2";
        disable_ligatures = "never";
        enable_audio_bell = false;
        # initial_window_height = 600;
        # initial_window_width = 1200;
        remember_window_size = "no";
        scrollback_lines = 10000;
        update_check_interval = 0;
        url_style = "curly";
        window_padding_width = 10;
        copy_on_select = "clipboard";
        strip_trailing_spaces = "smart";
        clipboard_control = "write-clipboard write-primary read-clipboard read-primary";
      };
      extraConfig = ''
        mouse_map right press ungrabbed paste_from_clipboard
      '';
    };
  };
}
