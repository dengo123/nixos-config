# modules/nixos/hardware/bluetooth/default.nix
{
  config,
  lib,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.hardware.bluetooth;
in {
  options.${namespace}.hardware.bluetooth = with types; {
    enable = mkBoolOpt false "Enable Bluetooth (BlueZ).";
    powerOnBoot = mkBoolOpt true "Power on the adapter at boot.";
    blueman = mkBoolOpt true "Enable Blueman integration.";
  };

  config = mkIf cfg.enable {
    hardware.bluetooth = {
      enable = true;
      powerOnBoot = cfg.powerOnBoot;
    };

    services.blueman.enable = cfg.blueman;
  };
}
