{
  config,
  lib,
  pkgs,
  ...
}: {
  fileSystems."/" = {
    device = "/dev/disk/by-label/ROOT";
    fsType = "ext4";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-label/BOOT";
    fsType = "vfat";
    options = ["nofail" "noauto"];
  };

  swapDevices = [
    {
      device = "/dev/disk/by-label/SWAP";
    }
  ];
}
