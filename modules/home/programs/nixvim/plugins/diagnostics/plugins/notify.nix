{...}: {
  programs.nixvim.config.plugins.notify = {
    enable = true;
    settings = {
      timeout = 3000;
      stages = "slide";
      render = "default";
      top_down = true;
    };
  };
}
