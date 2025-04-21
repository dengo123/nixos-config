{
  inputs,
  pkgs,
  lib,
  config,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.programs.nixvim;
in {
  options.${namespace}.programs.nixvim = with types; {
    enable = mkBoolOpt false "Enable nixvim as primary Neovim configuration.";
    mode = mkStrOpt "full" "Select nixvim mode: full, devshell, or lite.";
  };

  imports = [
    inputs.nixvim.homeManagerModules.nixvim
  ];

  config = mkIf cfg.enable (mkMerge [
    {
      programs.nixvim = {
        enable = true;
        defaultEditor = true;
        viAlias = true;
        vimAlias = true;
        extraPackages = with pkgs; [wl-clipboard];
      };

      home.file.".config/nvim/doc/nixvim.txt".source = ./nixvim.txt;
    }

    (mkIf (cfg.mode == "full") {
      ${namespace}.programs.nixvim.plugins = {
        alpha = enabled;
        cmp = enabled;
        dap = enabled;
        diagnostics = enabled;
        lsp = enabled;
        none-ls = enabled;
        telescope = enabled;
        textobjects = enabled;
        treesitter = enabled;
        ui = enabled;
        vim-options = enabled;
        neo-tree = enabled;
        surround = enabled;
        comment = enabled;
      };
    })

    (mkIf (cfg.mode == "devshell") {
      ${namespace}.programs.nixvim.plugins = {
        cmp = enabled;
        lsp = enabled;
        none-ls = enabled;
        telescope = enabled;
        vim-options = enabled;
        comment = enabled;
      };
    })

    (mkIf (cfg.mode == "lite") {
      ${namespace}.programs.nixvim.plugins = {
        vim-options = enabled;
      };
    })
  ]);
}
