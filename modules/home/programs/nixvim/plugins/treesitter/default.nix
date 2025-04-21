{
  lib,
  config,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.programs.nixvim.plugins.treesitter;
in {
  options.${namespace}.programs.nixvim.plugins.treesitter = {
    enable = mkBoolOpt false "Enable Treesitter and core features.";
  };

  config = mkIf cfg.enable {
    programs.nixvim.config.plugins.treesitter = {
      enable = true;
      settings = {
        ensure_installed = "all";
        highlight.enable = true;
        indent.enable = true;
      };
    };
  };
}
