{
  lib,
  config,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.programs.nixvim.plugins.lsp;
in {
  options.${namespace}.programs.nixvim.plugins.lsp = with types; {
    enable = mkBoolOpt false "Enable LSP support in NixVim";
  };

  config = mkIf cfg.enable {
    programs.nixvim = {
      plugins = {
        lsp = {
          enable = true;

          servers = {
            lua_ls.enable = true;
            pyright.enable = true;
            nil_ls.enable = true;
          };
        };

        lspkind.enable = true;
      };

      keymaps = [
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

      extraPackages = with pkgs; [
        gcc
        python3
        nodejs
        cargo
        rustc
        cmake
        clang-tools
        lua-language-server
        nil
        pyright
      ];
    };
  };
}
