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
  options.${namespace}.services.xscreensaver = {
    enable = mkBoolOpt false "Enable XScreenSaver daemon (no auto-lock, no watcher).";
  };

  config = mkIf cfg.enable {
    # Startet xscreensaver in der Usersession
    services.xscreensaver = {
      enable = true;
      package = pkgs.xscreensaver;
    };

    # Paket in den PATH (für xscreensaver-command, xscreensaver-demo, etc.)
    home.packages = [pkgs.xscreensaver];
  };
}
