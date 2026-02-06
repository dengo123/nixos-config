# modules/nixos/hardware/amd/default.nix
{
  config,
  lib,
  namespace,
  ...
}:
with lib;
with lib.${namespace};
let
  cfg = config.${namespace}.hardware.amd;
in
{
  options.${namespace}.hardware.amd = with types; {
    enable = mkBoolOpt false "Enable AMD GPU settings";
  };

  config = mkIf cfg.enable {
    services.xserver.videoDrivers = [ "amdgpu" ];

    boot.kernelParams = [
      "amdgpu.si_support=1"
      "amdgpu.cik_support=1"
    ];
  };
}
