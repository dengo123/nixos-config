{...}: {
  programs.nixvim.plugins = {
    lsp = {
      enable = true;

      servers = {
        lua_ls.enable = true;
        pyright.enable = true;
        ts_ls.enable = true;
        html.enable = true;
        nil_ls.enable = true;
      };
    };
  };

  programs.nixvim.keymaps = [
    {
      mode = "n";
      key = "<leader>ld";
      action = "<cmd>lua vim.lsp.buf.definition()<CR>";
      options.desc = "LSP: Go to definition";
    }
    {
      mode = "n";
      key = "<leader>lr";
      action = "<cmd>lua vim.lsp.buf.references()<CR>";
      options.desc = "LSP: Show references";
    }
    {
      mode = "n";
      key = "<leader>lh";
      action = "<cmd>lua vim.lsp.buf.hover()<CR>";
      options.desc = "LSP: Hover";
    }
    {
      mode = "n";
      key = "<leader>ln";
      action = "<cmd>lua vim.lsp.buf.rename()<CR>";
      options.desc = "LSP: Rename symbol";
    }
    {
      mode = "n";
      key = "<leader>lc";
      action = "<cmd>lua vim.lsp.buf.code_action()<CR>";
      options.desc = "LSP: Code action";
    }
    {
      mode = "n";
      key = "<leader>lf";
      action = "<cmd>lua vim.lsp.buf.format({ async = true })<CR>";
      options.desc = "LSP: Format buffer";
    }
  ];
}
