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

  # =========================
  # EDIDs
  # =========================

  EDID_TV = "00ffffffffffff0052620b0101010101ff150103805932780a0dc9a05747982712484c2fcf0081800101010101010101010101010101023a80d072382d40102c458075f23100001e662150b051001b304070360072f23100001e000000fc00544f53484942412d54560a2020000000fd00174c0f510f000a202020202020014002032c70521f1020212213041405121103021615070601260907071507c06d030c001000382dc01f1f1f1f008c0ad090204031200c40550098f2210000180e1f008051001e30408037003ef23100001cf12700a051002530508037003ef23100001ca91a00a050001630302037003ef23100001a0000000000000000000000fc";

  EDID_HP_HORZ = "00ffffffffffff0022f0793200000000021b0104a5331d783ac075a756529b260f5054210800d1c0a9c081c0b3009500810081800101023a801871382d40582c4500fd1e1100001e000000fd00323c1e5011000a202020202020000000fc00485020453233320a2020202020000000ff0033435137303231384e4b0a202000c2";

  EDID_HP_VERT = "00ffffffffffff0022f0793200000000021b0104a5331d783ac075a756529b260f5054210800d1c0a9c081c0b3009500810081800101023a801871382d40582c4500fd1e1100001e000000fd00323c1e5011000a202020202020000000fc00485020453233320a2020202020000000ff00334351373032304232300a202000f0";
in
{
  options.${namespace}.services.autorandr = {
    enable = mkEnableOption "Enable autorandr (X11 only)";
  };

  config = mkIf cfg.enable {

    programs.autorandr = {
      enable = true;

      profiles = {

        # =========================================================
        # dGPU WORK
        # =========================================================
        dgpu-work = {
          fingerprint = {
            "DP-4" = EDID_HP_HORZ;
            "DP-2" = EDID_HP_VERT;
            "HDMI-0" = EDID_TV;
          };

          config = {
            "DP-2" = {
              enable = true;
              mode = "1920x1080";
              rate = "60.00";
              rotate = "left";
              position = "0x0";
            };

            "DP-4" = {
              enable = true;
              primary = true;
              mode = "1920x1080";
              rate = "60.00";
              position = "1080x420";
            };

            # HDMI gespiegelt auf Main
            "HDMI-0" = disabled;
            "DP-0" = disabled;
            "DP-1" = disabled;
            "DP-3" = disabled;
            "DP-5" = disabled;
          };
        };

        # =========================================================
        # dGPU GAMING
        # =========================================================
        dgpu-gaming = {
          fingerprint = {
            "HDMI-0" = EDID_TV;
            "DP-2" = EDID_HP_VERT;
            "DP-4" = EDID_HP_HORZ;
          };

          config = {
            "HDMI-0" = {
              enable = true;
              primary = true;
              mode = "3840x2160";
              rate = "60.00";
              position = "0x0";
            };

            "DP-2" = {
              enable = true;
              mode = "1920x1080";
              rate = "60.00";
              rotate = "left";
              position = "3840x0";
            };

            "DP-4" = disabled;
            "DP-0" = disabled;
            "DP-1" = disabled;
            "DP-3" = disabled;
            "DP-5" = disabled;
          };
        };

        # =========================================================
        # iGPU MODE
        # =========================================================
        igpu = {
          fingerprint = {
            "DisplayPort-0" = EDID_HP_HORZ;
            "HDMI-A-0" = EDID_HP_VERT;
          };

          config = {
            "HDMI-A-0" = {
              enable = true;
              mode = "1920x1080";
              rate = "60.00";
              rotate = "left";
              position = "0x0";
            };

            "DisplayPort-0" = {
              enable = true;
              primary = true;
              mode = "1920x1080";
              rate = "60.00";
              position = "1080x420";
            };
          };
        };
      };
    };

    home.packages = [
      pkgs.autorandr
      pkgs.xorg.xrandr
    ];
  };
}
