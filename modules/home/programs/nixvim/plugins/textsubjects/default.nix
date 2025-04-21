{
  lib,
  config,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.programs.nixvim.plugins.textsubjects;
in {
  options.${namespace}.programs.nixvim.plugins.textsubjects = {
    enable = mkBoolOpt false "Enable Treesitter textsubjects";
  };

  config = mkIf cfg.enable {
    programs.nixvim.config.extraPlugins = [
      pkgs.vimPlugins.nvim-treesitter-textsubjects
    ];

    programs.nixvim.config.extraConfigLua = lib.mkAfter ''
      require("nvim-treesitter.configs").setup {
        textsubjects = {
          enable = true,
          keymaps = {
            ["<cr>"] = "textsubjects.smart",
            ["."] = "textsubjects.container-outer",
            ["i."] = "textsubjects.container-inner",
          },
        }
      }
    '';
  };
}
