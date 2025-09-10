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
  options.${namespace}.desktop.awesome = {
    enable = mkEnableOption "Enable Awesome WM (dynamic WM for X11)";
  };

  config = mkIf cfg.enable {
    xsession = {
      enable = true;

      windowManager.awesome = {
        enable = true;
        package = pkgs.awesome; # optional, aber explizit
      };

      # optional: beim X-Start noch Dinge tun
      # initExtra = '' ${pkgs.autorandr}/bin/autorandr --change || true '';
    };
  };
}
