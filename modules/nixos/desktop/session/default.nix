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

    # Greeter-Optionen (landen in extraConfig)
    greeter.activeMonitor =
      mkOpt str "primary"
      "Monitor for login box (primary | output name | index).";
    greeter.position = mkOpt str "50%x50%" "Login box position (e.g. 50%x50%).";

    # Greeter-Themes (wir setzen Namen via extraConfig, damit versionssicher)
    greeter.iconTheme = mkOpt str "Papirus-Dark" "Icon theme name for greeter.";
    greeter.cursorTheme = mkOpt str "Bibata-Original-Ice" "Cursor theme name for greeter.";
    greeter.cursorSize = mkOpt int 24 "Cursor size for greeter.";

    # Monitor-Layout (für deinen Output-Stand von xrandr)
    monitors.primary.output = mkOpt str "DP-4" "Primary output name (landscape).";
    monitors.primary.mode = mkOpt str "1920x1080" "Primary mode.";
    monitors.primary.rotate = mkOpt str "normal" "Rotation: normal|left|right|inverted.";
    monitors.primary.pos = mkOpt str "1080x420" "Position of primary (xposx ypos)";

    monitors.portrait.output = mkOpt str "DP-2" "Portrait output name (left-rotated).";
    monitors.portrait.mode = mkOpt str "1920x1080" "Base mode before rotation.";
    monitors.portrait.rotate = mkOpt str "left" "Rotation for portrait screen.";
    monitors.portrait.pos = mkOpt str "0x0" "Position of portrait (xposx ypos)";
  };

  config = mkIf cfg.enable {
    services.xserver.enable = true;

    # LightDM + unsere XSession
    services.xserver.displayManager = {
      lightdm.enable = true;

      sessionPackages = [xsessionPkg];
      defaultSession = "xsession";

      # Greeter: robuste INI-Konfig über extraConfig (kompatibel über Releases)
      lightdm.greeters.gtk = {
        enable = true;
        extraConfig = ''
          active-monitor=${cfg.greeter.activeMonitor}
          position=${cfg.greeter.position}
          icon-theme-name=${cfg.greeter.iconTheme}
          cursor-theme-name=${cfg.greeter.cursorTheme}
          cursor-theme-size=${toString cfg.greeter.cursorSize}
          # Falls du ein GTK-Theme explizit willst (optional):
          # theme-name=Adwaita-dark
        '';
      };

      # Monitor-Layout VOR dem Greeter fixen (aus deinem xrandr-Output übernommen)
      setupCommands = ''
        # Portrait links: DP-2, 1080x1920 @ (0,0)
        ${XR} --output ${cfg.monitors.portrait.output} --mode ${cfg.monitors.portrait.mode} \
              --rotate ${cfg.monitors.portrait.rotate} --pos ${cfg.monitors.portrait.pos}

        # Landscape rechts (Primär): DP-4, 1920x1080 @ (1080,420)
        ${XR} --output ${cfg.monitors.primary.output} --primary --mode ${cfg.monitors.primary.mode} \
              --rotate ${cfg.monitors.primary.rotate} --pos ${cfg.monitors.primary.pos}

        # Sicherheit: alles andere aus
        ${XR} --output HDMI-0 --off || true
        ${XR} --output DP-0   --off || true
        ${XR} --output DP-1   --off || true
        ${XR} --output DP-3   --off || true
        ${XR} --output DP-5   --off || true
      '';
    };

    # Stelle sicher, dass Icon- und Cursor-Themes verfügbar sind (für den Greeter)
    environment.systemPackages = with pkgs; [
      papirus-icon-theme
      bibata-cursors
    ];
  };
}
