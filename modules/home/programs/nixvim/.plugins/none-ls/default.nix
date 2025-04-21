{...}: {
  programs.nixvim.plugins.none-ls = {
    enable = true;

    sources = {
      formatting = {
        prettier = {
          enable = true;
          disableTsServerFormatter = true;
        };
        black.enable = true;
        isort.enable = true;
        stylua.enable = true;
        alejandra.enable = true;
      };
    };

    settings.on_attach = {
      __raw = ''
        function(client, bufnr)
          if client.supports_method("textDocument/formatting") then
            local group = vim.api.nvim_create_augroup("LspFormatting", { clear = true })

            vim.api.nvim_clear_autocmds({ group = group, buffer = bufnr })

            vim.api.nvim_create_autocmd("BufWritePre", {
              group = group,
              buffer = bufnr,
              callback = function()
                vim.lsp.buf.format({
                  bufnr = bufnr,
                  async = false,
                })
                vim.schedule(function()
                  vim.api.nvim_echo({
                    { "󰉼  Formatted with none-ls", "Comment" }
                  }, false, {})
                end)
              end,
            })
          end
        end
      '';
    };
  };
}
