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
      ragenix = disabled;
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
  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "24.05";
}
