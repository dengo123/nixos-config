{
  lib,
  config,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.programs.nixvim.plugins.treesitter-context;
in {
  options.${namespace}.programs.nixvim.plugins.treesitter-context = {
    enable = mkBoolOpt false "Enable sticky context display";
  };

  config = mkIf cfg.enable {
    programs.nixvim.config.plugins.treesitter-context.enable = true;

    programs.nixvim.config.keymaps = [
      {
        mode = "n";
        key = "<leader>tC";
        action = "<cmd>TSContextToggle<CR>";
        options.desc = "Treesitter: Toggle sticky context";
      }
    ];
  };
}
