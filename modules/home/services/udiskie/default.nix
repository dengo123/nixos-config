# modules/home/services/udiskie/default.nix
{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.services.udiskie;
in {
  options.${namespace}.services.udiskie = with types; {
    enable = mkBoolOpt false "Wether or not to enable udiskie services";
  };

  config = mkIf cfg.enable {
    services.udiskie = {
      enable = true;
      tray = "always";
      automount = true;
      notify = true;
      package = pkgs.udiskie;
    };
  };
}
