{ theme, ... }:

{
  programs.nixvim.config.plugins.notify = {
    enable = true;
    settings = {
      background_colour = theme.base00;
      timeout = 3000;
      stages = "slide";
      render = "default";
      top_down = true;
    };
  };
}

