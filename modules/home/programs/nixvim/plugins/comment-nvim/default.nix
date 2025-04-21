{
  lib,
  config,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.programs.nixvim.plugins.comment-nvim;
in {
  options.${namespace}.programs.nixvim.plugins.comment-nvim = {
    enable = mkBoolOpt false "Enable comment.nvim for easy commenting.";
  };

  config = mkIf cfg.enable {
    programs.nixvim.plugins.comment-nvim.enable = true;
  };
}
