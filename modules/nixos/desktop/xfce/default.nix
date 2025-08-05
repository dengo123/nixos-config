{
  config,
  lib,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.desktop.xfce;
in {
  options.${namespace}.desktop.xfce = with types; {
    enable = mkBoolOpt false "Enable XFCE desktop environment (theme installation only).";
  };

  config = mkIf cfg.enable {
    services.xserver = {
      enable = true;
      desktopManager.xfce.enable = true;
      displayManager.lightdm.enable = true;
    };
  };
}
