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
        luaModules = with pkgs.luaPackages; [
          lgi
        ];
      };
    };
  };
}
