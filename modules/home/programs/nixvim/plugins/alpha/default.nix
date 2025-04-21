{ ... }:

{
  programs.nixvim.plugins.alpha = {
    enable = true;

    layout = [
      {
        type = "padding";
        val = 4;
      }

      {
        type = "text";
        opts = {
          hl = "Type";
          position = "center";
        };
        val = [
          "███╗   ██╗██╗██╗  ██╗██╗   ██╗██╗███╗   ███╗"
          "████╗  ██║██║╚██╗██╔╝██║   ██║██║████╗ ████║"
          "██╔██╗ ██║██║ ╚███╔╝ ██║   ██║██║██╔████╔██║"
          "██║╚██╗██║██║ ██╔██╗ ╚██╗ ██╔╝██║██║╚██╔╝██║"
          "██║ ╚████║██║██╔╝ ██╗ ╚████╔╝ ██║██║ ╚═╝ ██║"
          "╚═╝  ╚═══╝╚═╝╚═╝  ╚═╝  ╚═══╝  ╚═╝╚═╝     ╚═╝"
        ];
      }

      {
        type = "padding";
        val = 8;
      }

      {
        type = "group";
        val = [
          {
            type = "button";
            val = "       New file   ";
            opts = {
              position = "center";
                shortcut = "n";
              keymap = [ "n" "n" ":ene | startinsert<CR>" ];
            };
          }
          { type = "padding"; val = 1; }

          {
            type = "button";
            val = "  󰮗     Find file  ";
            opts = {
              position = "center";
                shortcut = "f";
              keymap = [ "n" "f" ":Telescope find_files<CR>" ];
            };
          }
          { type = "padding"; val = 1; }

          {
            type = "button";
            val = "  󰄉     Recent     ";
            opts = {
              position = "center";
                shortcut = "r";
              keymap = [ "n" "r" ":Telescope oldfiles<CR>" ];
            };
          }
          { type = "padding"; val = 1; }

          {
            type = "button";
            val = "       Quit       ";
            opts = {
              position = "center";
                shortcut = "q";
              keymap = [ "n" "q" ":qa<CR>" ];
            };
          }
        ];
      }

      {
        type = "padding";
        val = 8;
      }

      {
        type = "text";
        opts = {
          hl = "Comment";
          position = "center";
        };
        val = [
          "┳┓┓┏  ┏┳┓┓┏┏┓  ┓ ┏┏┓┓┏"
          "┣┫┗┫   ┃ ┣┫┣   ┃┃┃┣┫┗┫"
          "┻┛┗┛   ┻ ┛┗┗┛  ┗┻┛┛┗┗┛"
        ];
      }

      {
        type = "padding";
        val = 4;
      }
    ];
  };
}

