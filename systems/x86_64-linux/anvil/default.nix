# systems/x86_64-linux/anvil/default.nix
{
  pkgs,
  lib,
  namespace,
  ...
}:
with lib;
with lib.${namespace};
{
  imports = [ ./hardware.nix ];

  networking.hostName = "anvil";

  nixforge = {
    config = {
      user = {
        name = "dengo123";
        fullName = "Deniz Altiok";
        email = "deniz060198@hotmail.com";
        extraGroups = [
          "networkmanager"
          "wheel"
          "audio"
          "video"
          "libvirtd"
          "docker"
          "i2c"
        ];
      };
    };
    bundles = {
      common = enabled;
      gpu.vendor = "nvidia";
      ai = enabled;
    };
    desktop.xsession = {
      enable = true;
      autoLogin = enabled;
    };
    hardware = {
      rgb = {
        enable = true;
        profile = mkForce "all_white.orp";
      };
    };
    programs = {
      steam = enabled;
      nh = enabled;
      nix-ld = enabled;
      screenshot = enabled;
    };
    services = {
      virtualisation = enabled;
      printing = enabled;
    };
    system = {
      boot.grub = enabled;
      keyboard = {
        layout = "us";
        variant = "intl";
        naturalScrolling = true;
      };
      region = {
        locale = "de_DE.UTF-8";
        timeZone = "Europe/Berlin";
      };
    };
  };
  system.stateVersion = "24.05";
}
