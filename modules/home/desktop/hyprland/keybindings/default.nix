{
  config,
  lib,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  mod = "SUPER";
  shiftMod = "SUPER SHIFT";

  cfg = config.${namespace}.desktop.hyprland.keybindings;

  terminal =
    if config.${namespace}.programs.ghostty.enable
    then "ghostty"
    else if config.${namespace}.programs.kitty.enable
    then "kitty"
    else "xterm";

  browser =
    if config.${namespace}.programs.zen.enable
    then "zen"
    else if config.${namespace}.programs.brave.enable
    then "brave"
    else "firefox";
in {
  options.${namespace}.desktop.hyprland.keybindings = with types; {
    enable = mkBoolOpt true "Enable Hyprland keybindings";
  };

  config = mkIf cfg.enable {
    wayland.windowManager.hyprland.settings = {
      bind = [
        # Programme starten
        "${mod},RETURN, exec, ${terminal}"
        "${mod},B, exec, ${browser}"
        "${mod},D, exec, launcher"
        "${mod},Y, exec, ${terminal} -e yazi"
        "${mod},O, exec, libreoffice"
        "${mod} CTRL,L, exec, hyprlock"

        # Dynamische Workspaces
        "${mod},up, workspace, +1"
        "${mod},down, workspace, -1"
        "${shiftMod},up, movetoworkspace, +1"
        "${shiftMod},down, movetoworkspace, -1"

        # Monitor-Fokus mit Pfeiltasten
        "${mod},left, focusmonitor, -1"
        "${mod},right, focusmonitor, 1"
        "${shiftMod},left, moveworkspacetomonitor, -1"
        "${shiftMod},right, moveworkspacetomonitor, 1"

        # Fensterfokus mit Pfeil-Tasten
        "${mod},H, movefocus, l"
        "${mod},L, movefocus, r"
        "${mod},K, movefocus, u"
        "${mod},L, movefocus, d"

        # Fenster verschieben Vim-Tasten
        "${shiftMod},H, swapwindow, l"
        "${shiftMod},L, swapwindow, r"
        "${shiftMod},K, swapwindow, u"
        "${shiftMod},J, swapwindow, d"

        # Fensterverwaltung
        "${mod},Q, killactive"
        "${shiftMod},Q, exit"
        "${mod},T, togglefloating"
        "${mod},F, fullscreen"
        "${mod},space, layoutmsg, orientationtoggle"
        "${mod},TAB, cyclenext"
        "${mod},TAB, bringactivetotop"

        # Spezial-Workspace (z.B. scratchpad)
        "${mod},S, togglespecialworkspace, magic"
        "${mod},S, movetoworkspace, special:magic"

        # Screenshots
        "${mod},PRINT, exec, screenshot window"
        ",PRINT, exec, screenshot monitor"
        "${shiftMod},PRINT, exec, screenshot region"
        "ALT,PRINT, exec, screenshot region swappy"
      ];

      bindm = [
        "${mod},mouse:272, movewindow"
        "${mod},R, resizewindow"
      ];

      bindl = [
        ",XF86AudioMute, exec, sound-toggle"
        ",switch:on:Lid Switch, exec, hyprctl keyword monitor 'eDP-1, disable'"
        ",switch:off:Lid Switch, exec, hyprctl keyword monitor 'eDP-1, preferred, auto, auto'"
      ];

      bindle = [
        ",XF86AudioRaiseVolume, exec, sound-up"
        ",XF86AudioLowerVolume, exec, sound-down"
        ",XF86MonBrightnessUp, exec, brightness-up"
        ",XF86MonBrightnessDown, exec, brightness-down"
      ];
    };
  };
}
