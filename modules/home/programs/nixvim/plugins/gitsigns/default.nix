{
  lib,
  config,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.programs.nixvim.plugins.gitsigns;
in {
  options.${namespace}.programs.nixvim.plugins.gitsigns = with types; {
    enable = mkBoolOpt false "Enable gitsigns.nvim for Git gutter integration";
  };

  config = mkIf cfg.enable {
    programs.nixvim.plugins.gitsigns = {
      enable = true;
      settings = {
        signs = {
          add = {text = "│";};
          change = {text = "│";};
          delete = {text = "_";};
          topdelete = {text = "‾";};
          changedelete = {text = "~";};
        };
      };
    };
  };
}
