{
  lib,
  config,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.programs.nixvim.plugins.noice;
in {
  options.${namespace}.programs.nixvim.plugins.noice = with types; {
    enable = mkBoolOpt false "Enable noice.nvim for enhanced UI notifications";
  };

  config = mkIf cfg.enable {
    programs.nixvim.plugins.noice = {
      enable = true;
      settings = {
        lsp.progress.enabled = true;
        messages.enabled = true;
        notify.enabled = true;
        cmdline.enabled = true;
        views.cmdline_popup.border.style = "rounded";
        presets = {
          bottom_search = true;
          command_palette = true;
          long_message_to_split = true;
        };
      };
    };
  };
}
