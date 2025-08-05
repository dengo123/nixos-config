{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.wm.dwm;
in {
  options.${namespace}.wm.dwm = with types; {
    enable = mkBoolOpt false "Enable DWM Window Manager";
  };

  config = mkIf cfg.enable {
    services.xserver.windowManager.dwm.enable = true;

    environment.systemPackages = with pkgs; [
      # DWM-Patches etc.
    ];
  };
}
