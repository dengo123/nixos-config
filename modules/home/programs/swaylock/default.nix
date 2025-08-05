{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.programs.swaylock;
in {
  options.${namespace}.programs.swaylock = with types; {
    enable = mkBoolOpt false "Enable swaylock-effects as lockscreen (minimal setup).";
  };

  config = mkIf cfg.enable {
    home.packages = [pkgs.swaylock-effects];
  };
}
