{
  lib,
  config,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.programs.nixvim.plugins.trouble;
in {
  options.${namespace}.programs.nixvim.plugins.trouble = with types; {
    enable = mkBoolOpt false "Enable trouble.nvim for diagnostics list";
  };

  config = mkIf cfg.enable {
    programs.nixvim = {
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
          options.desc = "Trouble: Show diagnostics";
        }
        {
          mode = "n";
          key = "<leader>tq";
          action = "<cmd>Trouble quickfix<CR>";
          options.desc = "Trouble: Quickfix list";
        }
        {
          mode = "n";
          key = "<leader>tl";
          action = "<cmd>Trouble loclist<CR>";
          options.desc = "Trouble: Location list";
        }
      ];
    };
  };
}
