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
    enable = mkBoolOpt false "Enable AMD GPU drivers";
    display = mkBoolOpt false "Allow AMD GPU to drive displays (Xorg)";
  };

  config = mkIf cfg.enable {
    hardware.graphics.enable = true;

    services.xserver.videoDrivers = mkIf cfg.display [ "amdgpu" ];
  };
}
