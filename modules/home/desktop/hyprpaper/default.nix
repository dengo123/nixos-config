{
  config,
  lib,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.desktop.hyprpaper;
in {
  options.${namespace}.desktop.hyprpaper = with types; {
    enable = mkBoolOpt false "Enable programs.hyprpaper";
  };

  config = mkIf cfg.enable {
    services.hyprpaper = {
      enable = true;
      settings = {
        ipc = "on";
        splash = false;
        splash_offset = 2.0;
      };
    };
  };
}
