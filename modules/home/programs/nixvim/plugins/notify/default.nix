{
  lib,
  config,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.programs.nixvim.plugins.notify;
in {
  options.${namespace}.programs.nixvim.plugins.notify = with types; {
    enable = mkBoolOpt false "Enable notify.nvim for notifications";
  };

  config = mkIf cfg.enable {
    programs.nixvim.plugins.notify = {
      enable = true;
      settings = {
        timeout = 3000;
        stages = "slide";
        render = "default";
        top_down = true;
      };
    };
  };
}
