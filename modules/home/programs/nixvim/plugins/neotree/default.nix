{
  lib,
  config,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.programs.nixvim.plugins.neo-tree;

  luaConfig = ''
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

    vim.keymap.set("n", "<leader>n", ":Neotree toggle<CR>", { noremap = true, silent = true })
    vim.keymap.set("n", "<leader>bf", ":Neotree buffers reveal float<CR>", { noremap = true, silent = true })

    vim.cmd("highlight NeoTreeNormal guibg=NONE ctermbg=NONE")
    vim.cmd("highlight NeoTreeNormalNC guibg=NONE ctermbg=NONE")
    vim.cmd("highlight NeoTreeEndOfBuffer guibg=NONE ctermbg=NONE")
    vim.cmd("highlight NeoTreeWinSeparator guibg=NONE ctermbg=NONE")
  '';
in {
  options.${namespace}.programs.nixvim.plugins.neo-tree = {
    enable = mkBoolOpt false "Enable Neo-tree file explorer";
  };

  config = mkIf cfg.enable {
    programs.nixvim.plugins.neo-tree.enable = true;
    programs.nixvim.extraConfigLua = lib.mkAfter luaConfig;
  };
}
