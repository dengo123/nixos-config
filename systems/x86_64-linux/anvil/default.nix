{
  lib,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; {
  imports = [./hardware.nix];

  networking.hostName = "anvil";

  ${namespace} = {
    config = {
      user = {
        name = "dengo123";
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
    desktop.hyprland = enabled;
    hardware = {
      bluetooth = enabled;
      cuda = enabled;
      nvidia = enabled;
    };
    programs = {
      steam = enabled;
      nh = enabled;
      nix-ld = enabled;
      ragenix = enabled;
    };
    services = {
      ollama = enabled;
      virtualisation = enabled;
    };
    system = {
      boot.grub = enabled;
      keyboard = {
        layout = "us";
        variant = "intl";
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
