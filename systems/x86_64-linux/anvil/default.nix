# systems/x86_64-linux/anvil/default.nix
{
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; {
  imports = [./hardware.nix];

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
      desktop = enabled;
      security = enabled;
      gpu.vendor = "nvidia";
      ai = enabled;
      gaming = enabled;
    };

    desktop.awesome = {
      enable = true;
      package = "patched";
      autoLogin = enabled;
    };

    hardware = {
      xmonitors = enabled;

      rgb = {
        enable = true;
        mode = "profile";
        profile = mkForce "all_white.orp";
      };
    };

    programs = {
      nh = enabled;
      nix-ld = enabled;
      screenshot = enabled;
    };

    services = {
      virtualisation = enabled;
    };

    system = {
      boot = {
        loader = "grub";
        timeout = 7;

        extraKernelParams = [
          "resume=UUID=5c987df5-d144-43ae-9db1-899a7d6f5424"
        ];

        # grub = {
        #   useOSProber = true;
        # };
      };
      keyboard = {
        layout = "us";
        variant = "intl";
        naturalScrolling = true;
      };

      region = {
        locale = "de_DE.UTF-8";
        timeZone = "Europe/Berlin";
      };

      # fonts = {
      #   packages.extra = with pkgs; [];
      #   fontconfig.defaults = {
      #     sansSerif = [];
      #     serif = [];
      #     monospace = [];
      #     emoji = [];
      #   };
      #   gtk.fontSize = 12;
      # };
    };
  };
  system.stateVersion = "24.05";
}
