{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.hardware.bluetooth;
in {
  options.${namespace}.hardware.bluetooth = with types; {
    enable = mkBoolOpt false "Enable Bluetooth (BlueZ) + Blueman.";
    powerOnBoot = mkBoolOpt true "Power on adapter at boot.";
    trayApplet = mkBoolOpt true "Install blueman (tools & applet binaries).";
  };

  config = mkIf cfg.enable {
    hardware.bluetooth = {
      enable = true;
      powerOnBoot = cfg.powerOnBoot;
    };

    services.blueman = enabled;

    environment.systemPackages = mkIf cfg.trayApplet [
      pkgs.obexd
      pkgs.blueman
    ];
  };
}
