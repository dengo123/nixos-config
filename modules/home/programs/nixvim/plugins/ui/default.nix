{
  lib,
  config,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.programs.nixvim.plugins.ui;
in {
  options.${namespace}.programs.nixvim.plugins.ui = {
    enable = mkBoolOpt false "Enable UI plugins like lualine, indentlines, devicons, comment, and surround.";
  };

  config = mkIf cfg.enable {
    programs.nixvim = {
      config = {
        options = {
          number = true;
          relativenumber = true;
        };

        plugins = {
          web-devicons.enable = true;

          lualine = {
            enable = true;
            settings = {
              opts = {
                theme = "auto";
                icons_enabled = true;
                component_separators = {
                  left = "";
                  right = "";
                };
                section_separators = {
                  left = "";
                  right = "";
                };
              };
              sections = {
                lualine_a = ["mode"];
                lualine_b = ["branch" "diff" "diagnostics"];
                lualine_c = ["filename"];
                lualine_x = ["encoding" "fileformat" "filetype"];
                lualine_y = ["progress"];
                lualine_z = ["location"];
              };
            };
          };

          indent-blankline = {
            enable = true;
          };
        };
      };
    };
  };
}
