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
    greeter.activeMonitor = mkOpt str "primary" "Monitor for login box.";
    greeter.position = mkOpt str "50%x50%" "Login box position (e.g. 50%x50%).";
    greeter.iconTheme = mkOpt str "Papirus-Dark" "Greeter icon theme.";
    greeter.cursorTheme = mkOpt str "Bibata-Original-Ice" "Greeter cursor theme.";
    greeter.cursorSize = mkOpt int 24 "Greeter cursor size.";
    greeter.backgroundColor = mkOpt str "#235CDB" "Greeter fallback background color.";
    greeter.backgroundImage =
      mkOpt (nullOr str) null
      "Optional wallpaper path; overrides background color.";

    # Monitors
    monitors.primary.output = mkOpt str "DP-4" "Primary output";
    monitors.primary.mode = mkOpt str "1920x1080" "Primary mode";
    monitors.primary.rotate = mkOpt str "normal" "Primary rotation";
    monitors.primary.pos = mkOpt str "1080x420" "Primary position";

    monitors.portrait.output = mkOpt str "DP-2" "Portrait output";
    monitors.portrait.mode = mkOpt str "1920x1080" "Portrait base mode";
    monitors.portrait.rotate = mkOpt str "left" "Portrait rotation";
    monitors.portrait.pos = mkOpt str "0x0" "Portrait position";

    # LightDM-gestütztes Locking via light-locker (nur An/Aus)
    lightLocker.enable = mkBoolOpt false "Start light-locker (locks via LightDM after the X11 screensaver activates).";
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
        '';
      };

      # Monitor layout before greeter
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

    # Optional: autologin
    services.xserver.displayManager.lightdm.extraConfig = mkIf cfg.autoLogin.enable ''
      autologin-user=${cfg.autoLogin.user}
      autologin-user-timeout=0
      allow-guest=false
      greeter-show-manual-login=true
    '';

    # Light-locker als systemd User-Service (keine zusätzlichen Optionen)
    systemd.user.services.light-locker = mkIf cfg.lightLocker.enable {
      description = "light-locker (LightDM lock after X11 screensaver activates)";
      after = ["graphical-session.target"];
      partOf = ["graphical-session.target"];
      wantedBy = ["graphical-session.target"];
      serviceConfig = {
        Environment = [
          "XDG_SESSION_TYPE=x11"
          "DISPLAY=:0"
          "XAUTHORITY=%h/.Xauthority"
        ];
        ExecStart = "${pkgs.lightlocker}/bin/light-locker";
        Restart = "on-failure";
        RestartSec = 2;
      };
    };

    environment.systemPackages = with pkgs; [
      papirus-icon-theme
      bibata-cursors
      lightlocker
    ];
  };
}
