{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.desktop.xfce;
in {
  options.${namespace}.desktop.xfce = with types; {
    enable = mkBoolOpt false "Enable XFCE with Windows XP Luna look.";
    variant = mkOption {
      type = types.enum [
        "blue"
        "silver"
        "olive"
      ];
      default = "blue";
      description = "Which Luna color variant to use (blue, silver, olive).";
    };
  };

  config = mkIf cfg.enable {
    services.xserver = {
      enable = true;
      desktopManager.xfce.enable = true;
      displayManager.lightdm.enable = true;
    };

    environment.systemPackages = with pkgs; [
      # Luna Theme (B00merang Project)
      (pkgs.stdenv.mkDerivation {
        pname = "luna-theme";
        version = "1.0";
        src = pkgs.fetchFromGitHub {
          owner = "B00merang-Project";
          repo = "Windows-XP";
          rev = "master";
          sha256 = "1hpdj2kjc0rsgk3gf59cs9chyqz70xjfiq926lvr4mpcyh1i5dby";
        };
        installPhase = ''
          mkdir -p $out/share/themes
          cp -r Windows-XP* $out/share/themes/Luna
        '';
      })

      # Chicago95 Icons
      (pkgs.stdenv.mkDerivation {
        pname = "chicago95-icons";
        version = "1.0";
        src = pkgs.fetchFromGitHub {
          owner = "grassmunk";
          repo = "Chicago95";
          rev = "master";
          sha256 = "0jia7ysar92byczpfv3zvwviiahcghm0akc6jia90md4zf7lpass";
        };
        installPhase = ''
          mkdir -p $out/share/icons
          cp -r Chicago95/icons/* $out/share/icons/
        '';
      })

      dmz-cursor-theme
    ];

    # GTK-Einstellungen
    xdg.configFile."gtk-3.0/settings.ini".text = ''
      [Settings]
      gtk-theme-name=Luna-${cfg.variant}
      gtk-icon-theme-name=Chicago95
      gtk-cursor-theme-name=DMZ-White
      gtk-font-name=Sans 10
    '';

    # XFWM4 Fensterrahmen
    xdg.configFile."xfce4/xfconf/xfce-perchannel-xml/xfwm4.xml".text = ''
      <?xml version="1.0" encoding="UTF-8"?>
      <channel name="xfwm4" version="1.0">
        <property name="general" type="empty">
          <property name="theme" type="string" value="Luna-${cfg.variant}"/>
        </property>
      </channel>
    '';

    # Optional Panel-Farbe im Luna-Stil
    xdg.configFile."xfce4/xfconf/xfce-perchannel-xml/xfce4-panel.xml".text = ''
      <?xml version="1.0" encoding="UTF-8"?>
      <channel name="xfce4-panel" version="1.0">
        <property name="background-style" type="int" value="0"/>
        <property name="background-color" type="string" value="#3A6EA5"/> <!-- Luna Blue -->
      </channel>
    '';
  };
}
