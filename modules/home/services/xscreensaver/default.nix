# modules/home/services/xscreensaver/default.nix
{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.services.xscreensaver;
in {
  options.${namespace}.services.xscreensaver = with types; {
    enable = mkBoolOpt false "Enable XScreenSaver daemon.";
  };

  config = mkIf cfg.enable {
    services.xscreensaver = {
      enable = true;
      package = pkgs.xscreensaver;
    };

    home.packages = [
      pkgs.xscreensaver
    ];
  };
}
