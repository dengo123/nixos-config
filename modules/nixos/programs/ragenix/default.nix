{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.programs.ragenix;
in {
  options.${namespace}.programs.ragenix = {
    enable = mkBoolOpt false "Enable ragenix secrets manager";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [pkgs.ragenix];
  };
}
