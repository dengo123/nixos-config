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

  # Terminal resolver
  terminal =
    if config.${namespace}.programs.ghostty.enable
    then "ghostty"
    else if config.${namespace}.programs.kitty.enable
    then "kitty"
    else "foot"; # fallback

  # Browser resolver
  browser =
    if config.${namespace}.programs.zen.enable
    then "zen"
    else if config.${namespace}.programs.brave.enable
    then "brave"
    else "firefox"; # fallback
in {
  options.${namespace}.desktop.hyprland.keybindings = with types; {
    enable = mkBoolOpt true "Enable Hyprland keybindings";
  };

  config = mkIf cfg.enable {
    wayland.windowManager.hyprland.settings = {
      bind =
        [
          "${mod},RETURN, exec, ${terminal}"
          "${mod},B, exec, ${browser}"
          "${mod},O, exec, librewolf"
          "${mod},Y, exec, ${terminal} -e yazi"

          "${mod},L, exec, hyprlock"
          "${mod},X, exec, power-menu"
          "${mod},D, exec, launcher"
          "${shiftMod},SPACE, exec, hyprfocus-toggle"

          "${mod},Q, killactive,"
          "${shiftMod},Q, exit"
          "${mod},T, togglefloating,"
          "${mod},F, fullscreen"

          "${mod},left, movefocus, l"
          "${mod},right, movefocus, r"
          "${mod},up, movefocus, u"
          "${mod},down, movefocus, d"
          "${shiftMod},up, focusmonitor, -1"
          "${shiftMod},down, focusmonitor, 1"
          "${shiftMod},left, layoutmsg, addmaster"
          "${shiftMod},right, layoutmsg, removemaster"

          "${mod},S, togglespecialworkspace, magic"
          "${mod},S, movetoworkspace, +0"
          "${mod},S, togglespecialworkspace, magic"
          "${mod},S, movetoworkspace, special:magic"
          "${mod},S, togglespecialworkspace, magic"

          "${mod},TAB, cyclenext,"
          "${mod},TAB, bringactivetotop,"

          "${mod},PRINT, exec, screenshot window"
          ",PRINT, exec, screenshot monitor"
          "${shiftMod},PRINT, exec, screenshot region"
          "ALT,PRINT, exec, screenshot region swappy"
        ]
        ++ (builtins.concatLists (
          builtins.genList (
            i: let
              ws = i + 1;
            in [
              "${mod},code:1${toString i}, workspace, ${toString ws}"
              "${mod} SHIFT,code:1${toString i}, movetoworkspace, ${toString ws}"
            ]
          )
          9
        ));

      bindm = [
        "${mod},mouse:272, movewindow"
        "${mod},R, resizewindow"
      ];

      bindl = [
        ",XF86AudioMute, exec, sound-toggle"
        ",switch:on:Lid Switch, exec, hyprctl keyword monitor 'eDP-1, disable'"
        ",switch:off:Lid Switch, exec, hyprctl keyword monitor 'eDP-1, prefered, auto, auto'"
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
