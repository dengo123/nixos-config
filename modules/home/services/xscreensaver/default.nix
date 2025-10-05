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
    enable = mkBoolOpt false "Start a xscreensaver daemon in user session.";
  };

  config = mkIf cfg.enable {
    services.xscreensaver = {
      enable = true;
      package = pkgs.xscreensaver;
    };
  };
}
