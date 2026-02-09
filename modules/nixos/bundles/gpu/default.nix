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
          "dual"
        ]
      ))
      null "GPU vendor. If null, the bundle won't toggle vendor driver modules.";
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
        mesa-demos
        vulkan-tools
      ];

      # optional: sensors nutzbar machen (wenn du das eh willst)
      hardware.sensor.iio.enable = mkDefault true;

      ${namespace}.services.lact = enabled;
    }

    # --- Vendor routing (nur toggles; Details im jeweiligen Modul) ---

    (mkIf (cfg.vendor == null) {
      hardware.graphics = enabled;
      services.xserver.videoDrivers = mkDefault ["modesetting"];
    })

    (mkIf (cfg.vendor == "nvidia") {
      ${namespace}.hardware.nvidia = {
        enable = mkDefault true;
        open = true;
        package = "production";
        display = true;
      };
    })

    (mkIf (cfg.vendor == "amd") {
      ${namespace}.hardware.amd = {
        enable = mkDefault true;
      };
    })

    (mkIf (cfg.vendor == "dual") {
      ${namespace} = {
        hardware.amd = {
          enable = true;
        };

        hardware.nvidia = {
          enable = true;
          open = true;
          package = "production";
          display = mkForce false;

          prime = {
            # optional, weil auto bei vendor=="dual" eh true ist:
            # enable = true;

            mode = "offload";
            amdgpuBusId = "PCI:12:0:0";
            nvidiaBusId = "PCI:1:0:0";
            offloadCmd = true;
          };
        };
      };

      # optional / recommended:
      services.xserver.videoDrivers = mkForce ["amdgpu" "nvidia"];
    })
  ]);
}
