{ lib, ... }:

let
  lua = ''
    require("neo-tree").setup({
      close_if_last_window = true,
      enable_git_status = true,
      enable_diagnostics = true,
      filesystem = {
        follow_current_file = {
          enabled = true,
        },
        group_empty_dirs = true,
        filtered_items = {
          hide_dotfiles = false,
          hide_gitignored = false,
        },
        hijack_netrw_behavior = "open_current",
      },
      window = {
        width = 30,
      },
    })

    -- Keybindings
    vim.keymap.set("n", "<leader>n", ":Neotree toggle<CR>", { noremap = true, silent = true })
    vim.keymap.set("n", "<leader>bf", ":Neotree buffers reveal float<CR>", { noremap = true, silent = true })

    -- Transparenz
    vim.cmd("highlight NeoTreeNormal guibg=NONE ctermbg=NONE")
    vim.cmd("highlight NeoTreeNormalNC guibg=NONE ctermbg=NONE")
    vim.cmd("highlight NeoTreeEndOfBuffer guibg=NONE ctermbg=NONE")
    vim.cmd("highlight NeoTreeWinSeparator guibg=NONE ctermbg=NONE")
  '';
in {
  programs.nixvim.plugins.neo-tree.enable = true;
  programs.nixvim.extraConfigLua = lib.mkAfter lua;
}

