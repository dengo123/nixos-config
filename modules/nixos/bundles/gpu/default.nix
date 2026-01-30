{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace};
let
  cfg = config.${namespace}.bundles.gpu;
in
{
  options.${namespace}.bundles.gpu = with types; {
    enable = mkBoolOpt true "Enable GPU bundle (vendor routing + shared tooling).";

    vendor = mkOpt (types.nullOr (
      types.enum [
        "nvidia"
        "amd"
      ]
    )) null "GPU vendor. If null, the bundle won't toggle vendor driver modules.";
  };

  config = mkIf cfg.enable (mkMerge [
    # --- Shared / GPU-agnostic tooling (system-wide) ---
    {
      environment.systemPackages = with pkgs; [
        pciutils
        usbutils
        lm_sensors
        nvtopPackages.full
        hwinfo
        lact
      ];

      # optional: sensors nutzbar machen (wenn du das eh willst)
      hardware.sensor.iio.enable = mkDefault true;
    }

    # --- Vendor routing (nur toggles; Details im jeweiligen Modul) ---
    (mkIf (cfg.vendor == "nvidia") {
      ${namespace}.hardware.nvidia.enable = mkDefault true;
    })

    # (mkIf (cfg.vendor == "amd") {
    #   ${namespace}.hardware.amd.enable = mkDefault true;
    # })
  ]);
}
