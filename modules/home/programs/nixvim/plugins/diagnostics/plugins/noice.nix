{ ... }:

{
  programs.nixvim.config.plugins.noice = {
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
}

