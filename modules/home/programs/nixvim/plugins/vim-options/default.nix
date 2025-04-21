{
  lib,
  config,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.programs.nixvim.plugins.vim-options;
in {
  options.${namespace}.programs.nixvim.plugins.vim-options = {
    enable = mkBoolOpt false "Enable basic Vim options and keymaps.";
  };

  config = mkIf cfg.enable {
    programs.nixvim = {
      config = {
        options = {
          expandtab = true;
          tabstop = 2;
          softtabstop = 2;
          shiftwidth = 2;
          number = true;
          relativenumber = true;
        };

        globals = {
          mapleader = " ";
          maplocalleader = " ";
        };

        keymaps = [
          # Fenster-Navigation (horizontal & vertikal)
          {
            mode = "n";
            key = "<C-h>";
            action = "<C-w>h";
            options = {
              noremap = true;
              silent = true;
            };
          }
          {
            mode = "n";
            key = "<C-j>";
            action = "<C-w>j";
            options = {
              noremap = true;
              silent = true;
            };
          }
          {
            mode = "n";
            key = "<C-k>";
            action = "<C-w>k";
            options = {
              noremap = true;
              silent = true;
            };
          }
          {
            mode = "n";
            key = "<C-l>";
            action = "<C-w>l";
            options = {
              noremap = true;
              silent = true;
            };
          }

          # Tab-Navigation
          {
            mode = "n";
            key = "<C-t>";
            action = ":tabnew<CR>";
            options = {
              noremap = true;
              silent = true;
            };
          }
          {
            mode = "n";
            key = "<C-n>";
            action = ":tabnext<CR>";
            options = {
              noremap = true;
              silent = true;
            };
          }
          {
            mode = "n";
            key = "<C-b>";
            action = ":tabprevious<CR>";
            options = {
              noremap = true;
              silent = true;
            };
          }
          {
            mode = "n";
            key = "<C-w>";
            action = ":tabclose<CR>";
            options = {
              noremap = true;
              silent = true;
            };
          }
        ];
      };
    };
  };
}
