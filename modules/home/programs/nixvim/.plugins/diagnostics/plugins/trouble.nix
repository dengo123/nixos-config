{ ... }:

{
  programs.nixvim = {
    config = {
      plugins.trouble = {
        enable = true;
        settings = {
          auto_open = false;
          auto_close = false;
          auto_preview = true;
          use_icons = true;
        };
      };

      keymaps = [
        {
          mode = "n";
          key = "<leader>td";
          action = "<cmd>Trouble diagnostics<CR>";
          options.desc = "Trouble: LSP-Diagnosen anzeigen";
        }
        {
          mode = "n";
          key = "<leader>tq";
          action = "<cmd>Trouble quickfix<CR>";
          options.desc = "Trouble: Quickfix anzeigen";
        }
        {
          mode = "n";
          key = "<leader>tl";
          action = "<cmd>Trouble loclist<CR>";
          options.desc = "Trouble: Location-List anzeigen";
        }
      ];
    };
  };
}

