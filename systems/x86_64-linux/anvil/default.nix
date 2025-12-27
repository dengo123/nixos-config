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
        ];
      };
    };
    bundles = {
      common = enabled;
    };
    desktop.hyprland = {
      enable = false;
      mode = "full";
    };
    desktop.xsession = {
      enable = true;
      autoLogin = enabled;
    };
    hardware = {
      cuda = enabled;
      nvidia = {
        enable = true;
        open = true;
        package = "production";
      };
    };
    programs = {
      steam = enabled;
      nh = enabled;
      nix-ld = enabled;
      screenshot = enabled;
    };
    services = {
      ollama = {
        enable = true;
        package = pkgs.ollama-cuda;
      };
      virtualisation = enabled;
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
