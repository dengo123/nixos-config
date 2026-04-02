# modules/nixos/desktop/lightdm/default.nix
{
  config,
  lib,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.desktop.lightdm;
  cursorCfg = config.${namespace}.system.cursor;
  userName = config.${namespace}.config.user.name;
in {
  options.${namespace}.desktop.lightdm = with types; {
    enable = mkBoolOpt false "Enable LightDM.";

    autoLogin = {
      enable = mkBoolOpt false "Enable LightDM autologin.";
      user = mkStrOpt userName "Autologin user name.";
    };

    gtkGreeter = {
      enable = mkBoolOpt true "Enable LightDM GTK greeter.";
      activeMonitor = mkOpt (nullOr str) null "Preferred active monitor for the GTK greeter.";
    };
  };

  config = mkIf cfg.enable {
    services.xserver.displayManager.lightdm.enable = true;

    services.displayManager.autoLogin = {
      enable = cfg.autoLogin.enable;
      user = cfg.autoLogin.user;
    };

    services.xserver.displayManager.lightdm.greeters.gtk = mkIf cfg.gtkGreeter.enable {
      enable = true;
      extraConfig =
        ''
          cursor-theme-name=${cursorCfg.name}
          cursor-theme-size=${toString cursorCfg.size}
        ''
        + optionalString (cfg.gtkGreeter.activeMonitor != null) ''
          active-monitor=${cfg.gtkGreeter.activeMonitor}
        '';
    };
  };
}
