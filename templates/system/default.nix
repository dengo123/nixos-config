{
  lib,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; {
  imports = [./hardware.nix];

  networking.hostName = "your-hostname"; # TODO: Change me!

  ${namespace} = {
    config = {
      user = {
        name = "your-username"; # TODO: Change me!
        extraGroups = [
          "networkmanager"
          "wheel"
        ];
      };
    };

    bundles = {
      common = enabled;
    };

    desktop.hyprland = disabled; # TODO: Set to enabled if needed

    hardware = {
      bluetooth = disabled;
      cuda = disabled;
      nvidia = disabled;
    };

    programs = {
      steam = disabled;
    };

    system = {
      boot.grub = enabled;
      keyboard = {
        layout = "us";
        variant = "intl";
      };
      region = {
        locale = "en_US.UTF-8";
        timeZone = "UTC";
      };
    };
  };

  system.stateVersion = "24.05";
}
