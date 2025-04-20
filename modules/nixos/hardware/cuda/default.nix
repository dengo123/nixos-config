{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.hardware.cuda;
in {
  options.${namespace}.hardware.cuda = with types; {
    enable = mkBoolOpt false "Enable NVIDIA CUDA support";
    toolkit = mkOpt package pkgs.cudatoolkit "CUDA toolkit package to install";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      pciutils
      cfg.toolkit
    ];

    # Inherit video driver config from nvidia module
    services.xserver.videoDrivers = ["nvidia"];

    # Optional systemd service to ensure nvidia modules get loaded
    systemd.services.nvidia-control-devices = {
      wantedBy = ["multi-user.target"];
      serviceConfig.ExecStart = "${pkgs.linuxPackages.nvidia_x11.bin}/bin/nvidia-smi";
    };
  };
}
