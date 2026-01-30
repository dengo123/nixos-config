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
  };

  config = mkIf cfg.enable {
    hardware = {
      graphics.enable = true;

      nvidia = {
        open = cfg.open;
        package = resolvePackage cfg.package;
        modesetting.enable = true;
        powerManagement.enable = true;
      };
    };

    services.xserver.videoDrivers = [ "nvidia" ];

    boot = {
      kernelModules = [
        "nvidia"
        "nvidia_modeset"
        "nvidia_uvm"
        "nvidia_drm"
      ];
      kernelParams = [
        "nvidia_drm.modeset=1"
        "nvidia.NVreg_PreserveVideoMemoryAllocations=1"
      ];
      extraModprobeConfig = ''
        options nvidia_drm modeset=1
      '';
    };

    environment.systemPackages = with pkgs; [ nvtopPackages.nvidia ];
  };
}
