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
  userName = config.${namespace}.config.user.name;

  cursorCfg = config.${namespace}.system.cursor;
  xmonitorsCfg = config.${namespace}.hardware.xmonitors;

  greeterExtraConfig =
    ''
      cursor-theme-name=${cursorCfg.name}
      cursor-theme-size=${toString cursorCfg.size}
    ''
    + optionalString (cfg.gtkGreeter.activeMonitor != null) ''
      active-monitor=${cfg.gtkGreeter.activeMonitor}
    '';

  seatExtraConfig = optionalString xmonitorsCfg.enable ''
    display-setup-script=${xmonitorsCfg.script}
    greeter-setup-script=${xmonitorsCfg.script}
    session-setup-script=${xmonitorsCfg.script}
  '';
in {
  options.${namespace}.desktop.lightdm = with types; {
    enable = mkBoolOpt false "Enable LightDM.";

    autoLogin = {
      enable = mkBoolOpt false "Enable LightDM autologin.";
      user = mkStrOpt userName "Autologin user name.";
    };

    gtkGreeter = {
      enable = mkBoolOpt true "Enable the LightDM GTK greeter.";

      activeMonitor =
        mkOpt (nullOr str) null
        "Preferred active monitor for the GTK greeter. Leave null to let LightDM choose automatically.";
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
      extraConfig = greeterExtraConfig;
    };

    services.xserver.displayManager.lightdm.extraConfig = mkAfter ''
      [Seat:*]
      ${seatExtraConfig}
    '';
  };
}
