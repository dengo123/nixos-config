{
  lib,
  config,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.programs.nixvim.plugins.treesitter-playground;
in {
  options.${namespace}.programs.nixvim.plugins.treesitter-playground = {
    enable = mkBoolOpt false "Enable Treesitter playground viewer";
  };

  config = mkIf cfg.enable {
    programs.nixvim.config.extraPlugins = [pkgs.vimPlugins.playground];

    programs.nixvim.config.keymaps = [
      {
        mode = "n";
        key = "<leader>tt";
        action = "<cmd>TSPlaygroundToggle<CR>";
        options.desc = "Treesitter: Playground toggle";
      }
      {
        mode = "n";
        key = "<leader>tq";
        action = "<cmd>TSHighlightCapturesUnderCursor<CR>";
        options.desc = "Treesitter: Show capture under cursor";
      }
    ];

    programs.nixvim.config.extraConfigLua = lib.mkAfter ''
      require("nvim-treesitter.configs").setup {
        playground = {
          enable = true,
          updatetime = 25,
          persist_queries = false,
        }
      }
    '';
  };
}
