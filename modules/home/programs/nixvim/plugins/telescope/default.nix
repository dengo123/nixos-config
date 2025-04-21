{
  lib,
  config,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.programs.nixvim.plugins.telescope;
in {
  options.${namespace}.programs.nixvim.plugins.telescope = {
    enable = mkBoolOpt false "Enable Telescope fuzzy finder";
  };

  config = mkIf cfg.enable {
    programs.nixvim.config.plugins.telescope = {
      enable = true;

      settings.defaults = {
        layout_config.horizontal.preview_width = 0.5;
        sorting_strategy = "ascending";
        scroll_strategy = "limit";
        prompt_prefix = "🔍 ";
        selection_caret = " ";
      };
    };
  };
}
