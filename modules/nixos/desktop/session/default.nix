# modules/nixos/session/default.nix
{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.session;
in {
  options.${namespace}.session = with types; {
    enable = mkBoolOpt false "Enable X11 session managed by LightDM.";
    autoLogin.enable = mkBoolOpt false "Enable LightDM autologin into xsession.";
    autoLogin.user = mkOpt str "dengo123" "User for autologin.";
  };

  config = mkIf cfg.enable {
    services.xserver = {
      enable = true;

      displayManager = {
        lightdm.enable = true;

        # Wichtig: DM startet ~/.xsession (von Home-Manager verwaltet)
        defaultSession = "none+xsession";

        # Autologin (optional)
        autoLogin = mkIf cfg.autoLogin.enable {
          enable = true;
          user = cfg.autoLogin.user;
        };
      };
    };
  };
}
