# modules/nixos/hardware/nvidia/default.nix
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
  cfg = config.${namespace}.hardware.nvidia;

  nvidiaPackages = config.boot.kernelPackages.nvidiaPackages;

  resolvePackage =
    pkg:
    {
      stable = nvidiaPackages.stable;
      production = nvidiaPackages.production;
      latest = nvidiaPackages.latest;
      beta = nvidiaPackages.beta;
      vulkan_beta = nvidiaPackages.vulkan_beta;
    }
    .${pkg};
in
{
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
  };

  config = mkIf cfg.enable {
    # --- Kernel / CUDA / DRM layer ---
    hardware = {
      graphics.enable = true;

      nvidia = {
        open = cfg.open;
        package = resolvePackage cfg.package;
        modesetting.enable = cfg.display;
        powerManagement.enable = true;
      };
    };

    # --- Xorg layer ---
    services.xserver.videoDrivers = mkIf cfg.display [ "nvidia" ];

    # --- Boot / DRM ---
    boot = {
      blacklistedKernelModules = [ "nouveau" ];

      kernelParams = [
        "nvidia.NVreg_PreserveVideoMemoryAllocations=1"
        "modprobe.blacklist=nouveau"
        "nouveau.modeset=0"
      ]
      ++ lib.optional cfg.display "nvidia_drm.modeset=1"
      ++ lib.optional (!cfg.display) "nvidia_drm.modeset=0";

      extraModprobeConfig = ''
        options nvidia_drm modeset=${if cfg.display then "1" else "0"}
      '';
    };

    environment.systemPackages = with pkgs; [
      nvtopPackages.nvidia
    ];
  };
}
