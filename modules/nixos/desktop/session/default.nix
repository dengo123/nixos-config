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
  cfg = config.${namespace}.desktop.session;

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
    # WICHTIG: Session-Namen bereitstellen
    passthru.providedSessions = ["xsession"];
  };
in {
  options.${namespace}.desktop.session = with types; {
    enable = mkBoolOpt false "Enable X11 session managed by LightDM.";
    autoLogin.enable = mkBoolOpt false "Enable LightDM autologin into xsession.";
    autoLogin.user = mkOpt str "dengo123" "User for autologin.";
  };

  config = mkIf cfg.enable {
    services = {
      xserver = {
        enable = true;

        displayManager = {
          lightdm.enable = true;

          # Unsere benannte Session
          sessionPackages = [xsessionPkg];
          defaultSession = "xsession";
        };
      };
    };
  };
}
