{ lib, pkgs, ... }:

{
  programs.nixvim = {
    config = {
      extraPlugins = with pkgs.vimPlugins; [
        playground
        nvim-treesitter-textsubjects
      ];

      keymaps = [
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
        {
          mode = "n";
          key = "<leader>tC";
          action = "<cmd>TSContextToggle<CR>";
          options.desc = "Treesitter: Toggle sticky context";
        }
      ];

      extraConfigLua = lib.mkAfter ''
        require("nvim-treesitter.configs").setup {
          textsubjects = {
            enable = true,
            keymaps = {
              ["<cr>"] = "textsubjects.smart",
              ["."] = "textsubjects.container-outer",
              ["i."] = "textsubjects.container-inner",
            },
          },
          playground = {
            enable = true,
            updatetime = 25,
            persist_queries = false,
          }
        }
      '';
    };
  };
}

