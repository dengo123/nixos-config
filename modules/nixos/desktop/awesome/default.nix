{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace};

let
  cfg = config.${namespace}.desktop.awesome;

  userName = config.${namespace}.config.user.name or "dengo123";
in
{
  options.${namespace}.desktop.awesome = with types; {
    enable = mkBoolOpt false "Enable Awesome WM session managed by SDDM (X11).";

    autoLogin = {
      enable = mkBoolOpt false "Enable SDDM autologin into Awesome.";
      user = mkStrOpt userName "Autologin user name.";
    };

    theme = mkStrOpt "" "Optional SDDM theme name (empty = default).";
  };

  config = mkIf cfg.enable (mkMerge [
    {
      # X11 Basis
      services.xserver.enable = true;
      services.xserver.libinput.enable = true;

      # Awesome WM bereitstellen
      services.xserver.windowManager.awesome.enable = true;

      # SDDM als Display Manager
      services.xserver.displayManager.sddm.enable = true;

      # Wichtig: Awesome ist eine WM-Session => "none+awesome"
      services.displayManager.defaultSession = "none+awesome";

      # Autologin optional
      services.displayManager.autoLogin = {
        enable = cfg.autoLogin.enable;
        user = cfg.autoLogin.user;
      };
    }

    (mkIf (cfg.theme != "") {
      services.xserver.displayManager.sddm.theme = cfg.theme;
    })
  ]);
}
