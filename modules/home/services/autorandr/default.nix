# modules/home/services/autorandr/default.nix
{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.services.autorandr;
in {
  options.${namespace}.services.autorandr = {
    enable = mkEnableOption "Enable autorandr for automatic monitor profile switching (X11 only)";
  };

  config = mkIf cfg.enable {
    programs.autorandr = {
      enable = true;
    };
  };
}
