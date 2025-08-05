{
  config,
  lib,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.programs.riverwm;
in {
  options.${namespace}.programs.riverwm = {
    enable = mkEnableOption "Enable RiverWM (Wayland compositor + tiling/floating WM)";
  };

  config = mkIf cfg.enable {
    wayland.windowManager.river = {
      enable = true;
      # Config bewusst leer lassen → dynamisch per riverctl
    };
  };
}
