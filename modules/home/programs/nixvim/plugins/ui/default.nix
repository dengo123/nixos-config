{ ... }:

{
  # imports = [ ./colors.nix];

  programs.nixvim.config = {
    enable = true;

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
            component_separators = { left = ""; right = ""; };
            section_separators = { left = ""; right = ""; };
          };
          sections = {
            lualine_a = [ "mode" ];
            lualine_b = [ "branch" "diff" "diagnostics" ];
            lualine_c = [ "filename" ];
            lualine_x = [ "encoding" "fileformat" "filetype" ];
            lualine_y = [ "progress" ];
            lualine_z = [ "location" ];
          };
        };
      };
    };
  };
}

