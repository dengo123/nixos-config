{
  config,
  lib,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.services.autorandr;
in {
  options.${namespace}.services.autorandr = {
    enable = mkEnableOption "Enable autorandr for automatic monitor profile switching";
  };

  config = mkIf cfg.enable {
  programs.autorandr = {
    enable = true;

    profiles = {
      dual = {
        fingerprint = {
          "DP-2" = "00ffffffffffff0022f0793200000000021b0104a5331d783ac075a756529b260f5054210800d1c0a9c081c0b3009500810081800101023a801871382d40582c";
          "DP-4" = "00ffffffffffff0022f0793200000000021b0104a5331d783ac075a756529b260f5054210800d1c0a9c081c0b3009500810081800101023a801871382d40582c";
        };

        config = {
          "DP-2" = {
            enable = true;
            primary = false;
            mode = "1080x1920";
            position = "0x0";
            rate = "60.00";
            rotate = "left";
          };
          "DP-4" = {
            enable = true;
            primary = true;
            mode = "1920x1080";
            position = "1080x420";
            rate = "60.00";
            rotate = "normal";
          };
        };
      };
    };
  };
}
