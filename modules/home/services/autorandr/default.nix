# modules/home/services/autorandr/default.nix
{
  config,
  lib,
  namespace,
  pkgs,
  ...
}:
with lib;
with lib.${namespace};
let
  cfg = config.${namespace}.services.autorandr;
in
{
  options.${namespace}.services.autorandr = {
    enable = mkEnableOption "Enable autorandr for automatic monitor profile switching (X11 only)";
  };

  config = mkIf cfg.enable {
    programs.autorandr = {
      enable = true;

      profiles = {
        vert-l = {
          fingerprint = {
            "HDMI-0" =
              "00ffffffffffff0052620b0101010101ff150103805932780a0dc9a05747982712484c2fcf0081800101010101010101010101010101023a80d072382d40102c458075f23100001e662150b051001b304070360072f23100001e000000fc00544f53484942412d54560a2020000000fd00174c0f510f000a202020202020014002032c70521f1020212213041405121103021615070601260907071507c06d030c001000382dc01f1f1f1f008c0ad090204031200c40550098f2210000180e1f008051001e30408037003ef23100001cf12700a051002530508037003ef23100001ca91a00a050001630302037003ef23100001a0000000000000000000000fc";
            "DP-4" =
              "00ffffffffffff0022f0793200000000021b0104a5331d783ac075a756529b260f5054210800d1c0a9c081c0b3009500810081800101023a801871382d40582c4500fd1e1100001e000000fd00323c1e5011000a202020202020000000fc00485020453233320a2020202020000000ff0033435137303231384e4b0a202000c2";
            "DP-2" =
              "00ffffffffffff0022f0793200000000021b0104a5331d783ac075a756529b260f5054210800d1c0a9c081c0b3009500810081800101023a801871382d40582c4500fd1e1100001e000000fd00323c1e5011000a202020202020000000fc00485020453233320a2020202020000000ff00334351373032304232300a202000f0";
          };
          config = {
            "DP-4" = {
              enable = true;
              primary = true;
              mode = "1920x1080";
              rate = "60.00";
              position = "1080x420";
            };
            "HDMI-0" = {
              enable = true;
              mode = "1920x1080";
              rate = "60.00";
              position = "1080x420";
            };
            "DP-2" = {
              enable = true;
              mode = "1920x1080";
              rate = "60.00";
              position = "0x0";
              rotate = "left";
            };
            "DP-0" = {
              enable = false;
            };
            "DP-1" = {
              enable = false;
            };
            "DP-3" = {
              enable = false;
            };
            "DP-5" = {
              enable = false;
            };
          };
        };
      };

    };

    # (Optional) Tools parat haben
    home.packages = [ pkgs.xorg.xrandr ];
  };
}
