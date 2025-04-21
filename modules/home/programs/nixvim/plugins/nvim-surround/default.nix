{
  lib,
  config,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.programs.nixvim.plugins.surround;
in {
  options.${namespace}.programs.nixvim.plugins.surround = {
    enable = mkBoolOpt false "Enable nvim-surround for surround editing.";
  };

  config = mkIf cfg.enable {
    programs.nixvim.plugins.nvim-surround.enable = true;
  };
}
