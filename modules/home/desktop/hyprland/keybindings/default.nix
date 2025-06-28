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
    else "foot";

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
        "${mod},RETURN, exec, ${terminal}"
        "${mod},B, exec, ${browser}"
        "${mod},D, exec, launcher"
        "${mod},L, exec, hyprlock"
        "${mod},Q, killactive"
        "${shiftMod},Q, exit"
        "${mod},T, togglefloating"
        "${mod},F, fullscreen"
        "${mod},TAB, cyclenext"
        "${mod},TAB, bringactivetotop"

        # Dynamische Workspace-Navigation auf fokussiertem Monitor
        "${mod},N, workspace, +1"
        "${mod},B, workspace, -1"
        "${shiftMod},N, movetoworkspace, +1"
        "${shiftMod},B, movetoworkspace, -1"

        # Feste Workspaces 1–3
        "${mod},1, workspace, 1"
        "${mod},2, workspace, 2"
        "${mod},3, workspace, 3"
        "${shiftMod},1, movetoworkspace, 1"
        "${shiftMod},2, movetoworkspace, 2"
        "${shiftMod},3, movetoworkspace, 3"

        # Fokus und Monitorwechsel
        "${mod},up, movefocus, u"
        "${mod},down, movefocus, d"
        "${mod},left, movefocus, l"
        "${mod},right, movefocus, r"
        "${shiftMod},left, focusmonitor, -1"
        "${shiftMod},right, focusmonitor, 1"

        # Extras
        "${mod},Y, exec, ${terminal} -e yazi"
        "${mod},O, exec, libreoffice"
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
