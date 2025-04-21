{ ... }:

{
  programs.nixvim.config.plugins.telescope = {
    enable = true;

    settings = {
      defaults = {
        layout_config = {
          horizontal = {
            preview_width = 0.5;
          };
        };
        sorting_strategy = "ascending";
        scroll_strategy = "limit";
        prompt_prefix = "🔍 ";
        selection_caret = " ";
      };
    };
  };
}

