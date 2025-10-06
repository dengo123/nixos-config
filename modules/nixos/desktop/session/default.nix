# modules/nixos/desktop/session/default.nix
{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.desktop.xsession;

  xsessionPkg = pkgs.stdenvNoCC.mkDerivation {
    name = "xsession-desktop";
    dontUnpack = true;
    dontBuild = true;
    installPhase = ''
            mkdir -p $out/share/xsessions
            cat > $out/share/xsessions/xsession.desktop <<'EOF'
      [Desktop Entry]
      Name=XSession
      Comment=Run the user's ~/.xsession
      Exec=${pkgs.runtimeShell}/bin/sh -lc 'exec "$HOME/.xsession"'
      Type=Application
      EOF
    '';
    passthru.providedSessions = ["xsession"];
  };

  XR = "${pkgs.xorg.xrandr}/bin/xrandr";
in {
  options.${namespace}.desktop.xsession = with types; {
    enable = mkBoolOpt false "Enable X11 session managed by LightDM.";

    autoLogin.enable = mkBoolOpt false "Enable LightDM autologin into xsession.";
    autoLogin.user = mkOpt str "dengo123" "User for autologin.";

    # Greeter
    greeter.activeMonitor = mkOpt str "primary" "Monitor for login box (primary | name | index).";
    greeter.position = mkOpt str "50%x50%" "Login box position (e.g. 50%x50%).";
    greeter.iconTheme = mkOpt str "Papirus-Dark" "Greeter icon theme.";
    greeter.cursorTheme = mkOpt str "Bibata-Original-Ice" "Greeter cursor theme.";
    greeter.cursorSize = mkOpt int 24 "Greeter cursor size.";
    # Hintergrund: Farbe (#235CDB) oder optionales Bild überschreibt Farbe
    greeter.backgroundColor = mkOpt str "#235CDB" "Fallback background color for the greeter.";
    greeter.backgroundImage =
      mkOpt (nullOr str) null
      "Optional wallpaper path; overrides background color.";

    # Monitore
    monitors.primary.output = mkOpt str "DP-4" "Primary output";
    monitors.primary.mode = mkOpt str "1920x1080" "Primary mode";
    monitors.primary.rotate = mkOpt str "normal" "Primary rotation";
    monitors.primary.pos = mkOpt str "1080x420" "Primary position";

    monitors.portrait.output = mkOpt str "DP-2" "Portrait output";
    monitors.portrait.mode = mkOpt str "1920x1080" "Portrait base mode";
    monitors.portrait.rotate = mkOpt str "left" "Portrait rotation";
    monitors.portrait.pos = mkOpt str "0x0" "Portrait position";
  };

  config = mkIf cfg.enable {
    services.xserver.enable = true;

    services.xserver.displayManager = {
      lightdm.enable = true;
      sessionPackages = [xsessionPkg];
      defaultSession = "xsession";

      lightdm.greeters.gtk = {
        enable = true;
        extraConfig = ''
          active-monitor=${cfg.greeter.activeMonitor}
          position=${cfg.greeter.position}
          icon-theme-name=${cfg.greeter.iconTheme}
          cursor-theme-name=${cfg.greeter.cursorTheme}
          cursor-theme-size=${toString cfg.greeter.cursorSize}
          user-background=false
          background=${
            if cfg.greeter.backgroundImage != null
            then cfg.greeter.backgroundImage
            else cfg.greeter.backgroundColor
          }

          # Panel unten,  Links: Power | Mitte: leer | Rechts: Clock
          panel-position=bottom
          indicators=~power;~spacer;~clock
          clock-format=%H:%M
        '';
      };

      # Monitor-Layout vor dem Greeter
      setupCommands = ''
        ${XR} --output ${cfg.monitors.portrait.output} --mode ${cfg.monitors.portrait.mode} \
              --rotate ${cfg.monitors.portrait.rotate} --pos ${cfg.monitors.portrait.pos}
        ${XR} --output ${cfg.monitors.primary.output} --primary --mode ${cfg.monitors.primary.mode} \
              --rotate ${cfg.monitors.primary.rotate} --pos ${cfg.monitors.primary.pos}
        ${XR} --output HDMI-0 --off || true
        ${XR} --output DP-0   --off || true
        ${XR} --output DP-1   --off || true
        ${XR} --output DP-3   --off || true
        ${XR} --output DP-5   --off || true
      '';
    };

    # Panel/Leisten-CSS für den LightDM GTK Greeter
    environment.etc."lightdm/lightdm-gtk-greeter.css".text = ''
      #panel {
        background: #1A50B8;
        min-height: 32px;
        padding: 0;
      }
      #panel .indicator { padding: 0 8px; }
      #clock { font-weight: 600; }
    '';

    # Autologin (optional)
    services.xserver.displayManager.lightdm.extraConfig = mkIf cfg.autoLogin.enable ''
      autologin-user=${cfg.autoLogin.user}
      autologin-user-timeout=0
      allow-guest=false
      greeter-show-manual-login=true
    '';

    # Greeter-Assets
    environment.systemPackages = with pkgs; [
      papirus-icon-theme
      bibata-cursors
    ];
  };
}
