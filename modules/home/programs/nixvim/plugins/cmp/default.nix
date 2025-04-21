{ lib, ... }:

{
  programs.nixvim.plugins = {
    cmp = {
      enable = true;

      settings = {
        snippet.expand = {
          __raw = "function(args) require('luasnip').lsp_expand(args.body) end";
        };

        mapping = {
          "<C-b>" = "cmp.mapping.scroll_docs(-4)";
          "<C-f>" = "cmp.mapping.scroll_docs(4)";
          "<C-Space>" = "cmp.mapping.complete()";
          "<C-e>" = "cmp.mapping.abort()";

          "<CR>" = {
            __raw = "cmp.mapping.confirm({ select = true })";
          };

          "<Tab>" = {
            __raw = ''
              function(fallback)
                local cmp = require("cmp")
                local luasnip = require("luasnip")
                if cmp.visible() then
                  cmp.select_next_item()
                elseif luasnip.expand_or_jumpable() then
                  luasnip.expand_or_jump()
                else
                  fallback()
                end
              end
            '';
          };

          "<S-Tab>" = {
            __raw = ''
              function(fallback)
                local cmp = require("cmp")
                local luasnip = require("luasnip")
                if cmp.visible() then
                  cmp.select_prev_item()
                elseif luasnip.jumpable(-1) then
                  luasnip.jump(-1)
                else
                  fallback()
                end
              end
            '';
          };
        };

        formatting.format = 
          lib.mkForce {
          __raw = ''
            function(entry, vim_item)
              local lspkind = require("lspkind")
              return lspkind.cmp_format({
                mode = "symbol_text",
                maxwidth = 50,
                ellipsis_char = "...",
              })(entry, vim_item)
            end
          '';
        };

        sources = [
          { name = "nvim_lsp"; }
          { name = "luasnip"; }
          { name = "buffer"; }
          { name = "path"; }
          { name = "nvim_lua"; }
          { name = "cmdline"; }
        ];
      };
    };

    # Zusatzquellen & UI
    cmp-nvim-lsp.enable = true;
    cmp-buffer.enable = true;
    cmp-path.enable = true;
    cmp-cmdline.enable = true;
    cmp-nvim-lua.enable = true;

    # Snippet und Visualisierung
    luasnip.enable = true;
    friendly-snippets.enable = true;
    lspkind.enable = true;

    # Autopairs für automatische () nach <CR>
    nvim-autopairs = {
      enable = true;
      settings = {
        checkTs = true;
        map_cr = true;
      };
    };
  };
}

