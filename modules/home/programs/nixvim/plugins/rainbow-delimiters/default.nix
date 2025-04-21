{
  lib,
  config,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.programs.nixvim.plugins.rainbow-delimiters;
in {
  options.${namespace}.programs.nixvim.plugins.rainbow-delimiters = {
    enable = mkBoolOpt false "Enable rainbow bracket coloring";
  };

  config = mkIf cfg.enable {
    programs.nixvim.config.plugins.rainbow-delimiters.enable = true;
  };
}
