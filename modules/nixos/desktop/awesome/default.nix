# modules/nixos/desktop/awesome/default.nix
{
  config,
  lib,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.desktop.awesome;
  userName = config.${namespace}.config.user.name or "dengo123";
in {
  options.${namespace}.desktop.awesome = with types; {
    enable = mkBoolOpt false "Enable Awesome WM on X11.";

    autoLogin = {
      enable = mkBoolOpt false "Enable display-manager autologin into Awesome.";
      user = mkStrOpt userName "Autologin user name.";
    };

    # DM Auswahl bewusst simpel gehalten
    displayManager = mkOpt (types.enum ["lightdm" "sddm"]) "lightdm" "Display manager to use.";
  };

  config = mkIf cfg.enable {
    services.xserver.enable = true;
    services.xserver.windowManager.awesome.enable = true;
    services.libinput.enable = true;

    services.displayManager.defaultSession = "none+awesome";

    services.displayManager.autoLogin = {
      enable = cfg.autoLogin.enable;
      user = cfg.autoLogin.user;
    };

    # DM toggles (Theming kommt in separate Module)
    services.xserver.displayManager.lightdm.enable = cfg.displayManager == "lightdm";
    services.displayManager.sddm.enable = cfg.displayManager == "sddm";

    # optional: minimaler greeter, falls du theme getrennt hältst
    services.xserver.displayManager.lightdm.greeters.gtk.enable = mkIf (cfg.displayManager == "lightdm") (mkDefault true);
  };
}
