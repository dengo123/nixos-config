{
  config,
  lib,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.programs.hyprlock;
  foreground = "rgba(216, 222, 233, 0.70)";
  imageStr = "${config.stylix.image}";
  font = config.stylix.fonts.serif.name;
in {
  options.${namespace}.programs.hyprlock = with types; {
    enable = mkBoolOpt false "Enable programs.hyprlock";
  };

  config = mkIf cfg.enable {
    programs.hyprlock = {
      enable = true;
      settings = {
        general = {
          grace = 5;
          # deprecated: no_fade_in
          # deprecated: disable_loading_bar
        };

        background = {
          monitor = "";
          path = imageStr;
          blur_passes = 0;
          contrast = 0.8916;
          brightness = 0.7172;
          vibrancy = 0.1696;
          vibrancy_darkness = 0.0;
        };

        label = [
          {
            monitor = "";
            text = ''cmd[update:1000] echo -e "$(date +"%A, %B %d")"'';
            color = foreground;
            font_size = 28;
            font_family = font + " Bold";
            position = "0, 490";
            halign = "center";
            valign = "center";
          }
          {
            monitor = "";
            text = ''cmd[update:1000] echo "<span>$(date +"%I:%M")</span>"'';
            color = foreground;
            font_size = 160;
            font_family = "steelfish outline regular";
            position = "0, 370";
            halign = "center";
            valign = "center";
          }
          {
            monitor = "";
            text = "    $USER";
            color = foreground;
            font_size = 18;
            font_family = font + " Bold";
            position = "0, -180";
            halign = "center";
            valign = "center";
          }
        ];

        input-field = [
          {
            monitor = "";
            size = "300, 60";
            outline_thickness = 2;
            dots_size = 0.2;
            dots_spacing = 0.2;
            dots_center = true;
            outer_color = "rgba(255, 255, 255, 0)";
            inner_color = "rgba(255, 255, 255, 0.1)";
            font_color = foreground;
            fade_on_empty = false;
            font_family = font + " Bold";
            placeholder_text = "<i>🔒 Enter Password</i>";
            hide_input = false;
            position = "0, -250";
            halign = "center";
            valign = "center";
          }
        ];
      };
    };
  };
}
