# modules/nixos/desktop/awesome/default.nix
{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.desktop.awesome;
in {
  options.${namespace}.desktop.awesome = with types; {
    enable = mkBoolOpt false "Enable AwesomeWM on X11 with LightDM.";
    autoLogin.enable = mkBoolOpt false "Enable LightDM autologin into Awesome.";
    autoLogin.user = mkOpt str "dengo123" "Username for LightDM autologin.";
  };

  config = mkIf cfg.enable {
    services.xserver = {
      enable = true;
      xkb.layout = cfg.xkbLayout;

      # Display Manager: LightDM, ohne Theme/Greeter-Anpassungen
      displayManager = {
        lightdm.enable = true;
        defaultSession = "none+awesome";

        # Autologin (optional)
        autoLogin = mkIf cfg.autoLogin.enable {
          enable = true;
          user = cfg.autoLogin.user;
        };
      };

      # Window Manager: Awesome
      windowManager.awesome = {
        enable = true;
        package = pkgs.awesome;
      };
    };
  };
}
