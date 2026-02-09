# modules/nixos/hardware/nvidia/default.nix
{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.hardware.nvidia;

  # auto-prime trigger (like bundles/ai does it)
  primeMode = config.${namespace}.bundles.gpu.vendor or null;
  primeAuto = primeMode == "dual";

  primeEnable = cfg.prime.enable || primeAuto;

  nvidiaPackages = config.boot.kernelPackages.nvidiaPackages;

  resolvePackage = pkg:
    {
      stable = nvidiaPackages.stable;
      production = nvidiaPackages.production;
      latest = nvidiaPackages.latest;
      beta = nvidiaPackages.beta;
      vulkan_beta = nvidiaPackages.vulkan_beta;
    }
    .${
      pkg
    };
in {
  options.${namespace}.hardware.nvidia = with types; {
    enable = mkBoolOpt false "Enable NVIDIA drivers";

    package = mkOpt (enum [
      "stable"
      "production"
      "latest"
      "beta"
      "vulkan_beta"
    ]) "stable" "NVIDIA driver package to use";

    open = mkBoolOpt false "Use the NVIDIA open kernel module (experimental)";
    display = mkBoolOpt false "Allow NVIDIA to drive displays (KMS + Xorg)";

    prime = {
      enable = mkBoolOpt false ''
        Enable NVIDIA PRIME configuration (hybrid setups).
        Note: will be automatically enabled when bundles.gpu.vendor == "dual".
      '';

      mode =
        mkOpt (enum ["offload" "sync"]) "offload"
        "PRIME mode: offload (iGPU master + dGPU render) or sync (dGPU master).";

      amdgpuBusId = mkOpt (nullOr str) null "AMD iGPU BusID (e.g. PCI:12:0:0).";
      nvidiaBusId = mkOpt (nullOr str) null "NVIDIA dGPU BusID (e.g. PCI:1:0:0).";

      offloadCmd = mkBoolOpt true "Enable the nvidia-offload helper command.";
    };
  };

  config = mkIf cfg.enable (mkMerge [
    # --- sanity / invariants ---
    {
      assertions = [
        {
          assertion =
            (!primeEnable)
            || (
              cfg.prime.amdgpuBusId != null && cfg.prime.nvidiaBusId != null
            );
          message = "${namespace}.hardware.nvidia: PRIME is enabled (explicitly or via bundles.gpu.vendor == \"dual\"), but amdgpuBusId/nvidiaBusId are not set.";
        }
      ];
    }

    # --- Kernel / CUDA / DRM layer ---
    {
      hardware.graphics.enable = true;

      hardware.nvidia = {
        open = cfg.open;
        package = resolvePackage cfg.package;

        # If NVIDIA is allowed to drive displays, enable DRM KMS.
        # In dual/offload mode you typically want this false.
        modesetting.enable = cfg.display;

        powerManagement.enable = true;
      };

      # --- Boot / DRM ---
      boot = {
        blacklistedKernelModules = ["nouveau"];

        kernelParams =
          [
            "nvidia.NVreg_PreserveVideoMemoryAllocations=1"
            "modprobe.blacklist=nouveau"
            "nouveau.modeset=0"
          ]
          ++ lib.optional cfg.display "nvidia_drm.modeset=1"
          ++ lib.optional (!cfg.display) "nvidia_drm.modeset=0";

        extraModprobeConfig = ''
          options nvidia_drm modeset=${
            if cfg.display
            then "1"
            else "0"
          }
        '';
      };

      environment.systemPackages = with pkgs; [
        nvtopPackages.nvidia
      ];
    }

    # --- Xorg layer (only if NVIDIA drives displays) ---
    (mkIf cfg.display {
      services.xserver.videoDrivers = mkDefault ["nvidia"];
    })

    # --- PRIME (explicit or auto when gpu.vendor == "dual") ---
    (mkIf primeEnable {
      hardware.nvidia.prime = mkMerge [
        {
          amdgpuBusId = cfg.prime.amdgpuBusId;
          nvidiaBusId = cfg.prime.nvidiaBusId;
        }

        (mkIf (cfg.prime.mode == "offload") {
          offload.enable = true;
          offload.enableOffloadCmd = cfg.prime.offloadCmd;
        })

        (mkIf (cfg.prime.mode == "sync") {
          sync.enable = true;
        })
      ];
    })
  ]);
}
