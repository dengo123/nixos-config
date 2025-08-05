{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.misc.gtk;
in {
  options.${namespace}.misc.gtk = with types; {
    enable = mkBoolOpt false "Enable GTK and XFCE theming.";
    variant = mkOption {
      type = types.enum [
        "blue"
        "silver"
        "olive"
      ];
      default = "blue";
      description = "Choose Luna variant (blue, silver, olive)";
    };
  };

  config = mkIf cfg.enable {
    # GTK 3 Settings
    xdg.configFile."gtk-3.0/settings.ini".text = ''
      [Settings]
      gtk-theme-name=Luna-${cfg.variant}
      gtk-icon-theme-name=Chicago95
      gtk-cursor-theme-name=DMZ-White
      gtk-font-name=Sans 10
    '';

    # XFWM4 Theme
    xdg.configFile."xfce4/xfconf/xfce-perchannel-xml/xfwm4.xml".text = ''
      <?xml version="1.0" encoding="UTF-8"?>
      <channel name="xfwm4" version="1.0">
        <property name="general" type="empty">
          <property name="theme" type="string" value="Luna-${cfg.variant}"/>
        </property>
      </channel>
    '';

    # Optional: Panelfarbe XP-Luna-Style
    xdg.configFile."xfce4/xfconf/xfce-perchannel-xml/xfce4-panel.xml".text = ''
      <?xml version="1.0" encoding="UTF-8"?>
      <channel name="xfce4-panel" version="1.0">
        <property name="background-style" type="int" value="0"/>
        <property name="background-color" type="string" value="#3A6EA5"/>
      </channel>
    '';
  };
}
