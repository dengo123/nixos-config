# modules/nixos/bundles/gpu/default.nix
{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.bundles.gpu;
in {
  options.${namespace}.bundles.gpu = with types; {
    enable = mkBoolOpt true "Enable GPU bundle (vendor routing + shared tooling).";

    vendor =
      mkOpt (types.nullOr (
        types.enum [
          "nvidia"
          "amd"
          "nvidia-headless"
        ]
      ))
      null "GPU vendor/profile.";
  };

  config = mkIf cfg.enable (mkMerge [
    {
      environment.systemPackages = with pkgs; [
        pciutils
        usbutils
        lm_sensors
        nvtopPackages.full
        hwinfo
        mesa-demos
      ];

      hardware.sensor.iio.enable = mkDefault true;

      ${namespace}.services.lact = enabled;
    }

    (mkIf (cfg.vendor == null) {
      hardware.graphics.enable = mkDefault true;
      services.xserver.videoDrivers = mkDefault ["modesetting"];
    })

    (mkIf (cfg.vendor == "nvidia") {
      ${namespace}.hardware.nvidia = {
        enable = mkDefault true;
        open = true;
        package = "production";
      };
    })

    (mkIf (cfg.vendor == "amd") {
      ${namespace}.hardware.amd = {
        enable = mkDefault true;
      };
    })

    (mkIf (cfg.vendor == "nvidia-headless") {
      ${namespace}.hardware.nvidia = {
        enable = mkDefault true;
        open = true;
        package = "production";
        display = mkForce false;
      };
    })
  ]);
}
