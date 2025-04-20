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
        name = "hephaestus";
        fullName = "Deniz Altiok";
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
    desktop.kde = enabled;
    hardware = {
      bluetooth = enabled;
      nvidia = {
        enable = true;
        open = true;
        package = "beta";
      };
    };
    programs = {
      nh = enabled;
      nix-ld = enabled;
      ragenix = disabled;
    };
    services = {
      virtualisation = enabled;
    };
    system = {
      boot.systemd = enabled;
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
