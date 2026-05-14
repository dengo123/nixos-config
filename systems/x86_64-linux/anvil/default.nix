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
      desktop = enabled;
      security = enabled;
      gpu.vendor = "nvidia";
      ai = enabled;
      gaming = enabled;
    };

    # desktop.lightdm = {
    #   autoLogin.enable = true;
    # };

    hardware = {
      xmonitors = {
        enable = true;

        # framebuffer = {
        #   size = "3000x1920";
        # };

        outputs = {
          DP-2 = {
            enable = true;
            mode = "1920x1080";
            rate = 60;
            pos = "0x0";
            rotate = "left";
          };

          DP-4 = {
            enable = true;
            primary = true;
            mode = "1920x1080";
            rate = 60;
            pos = "1080x420";
            rotate = "normal";
          };
        };
      };

      audio = {
        gamingBuffer = true;
      };

      rgb = {
        enable = true;
        mode = "profile";
        profile = mkForce "off.orp";
      };
    };

    programs = {
      nix-ld = {
        enable = true;
        # libraries = with pkgs; [
        # ];
        # useSteamRunSet = true;
      };
    };

    services = {
      virtualisation = enabled;
      wireplumber = {
        sinks = {
          # headset = {
          #   priority = 1400;
          #   alsaNodeName = "alsa_output.usb-Logitech_G_series_G435_Wireless_Gaming_Headset_202105190004-00.analog-stereo";
          #   bluezNodeName = null;
          # };

          tv = {
            priority = 1300;
            alsaNodeName = "alsa_output.pci-0000_01_00.1.hdmi-stereo";
            bluezNodeName = null;
          };

          speakers = {
            priority = 1200;
            alsaNodeName = "alsa_output.usb-Generic_USB_Audio-00.HiFi__Speaker__sink";
            bluezNodeName = null;
          };
        };
      };
    };

    system = {
      boot = {
        loader = "grub";
        timeout = 7;
        quietBoot = true;

        extraKernelParams = [
          "resume=UUID=5c987df5-d144-43ae-9db1-899a7d6f5424"
          "usbcore.autosuspend=-1"
          "mt7921e.disable_aspm=1"
          "mt7921e.disable_pm=1"
          "pcie_aspm=off"
        ];

        # grub = {
        #   useOSProber = true;
        # };
      };

      systemd = {
        logLevel = "notice";
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

      # cursor = {
      #   package = pkgs.bibata-cursors;
      #   name = "Bibata-Original-Ice";
      #   size = 24;
      # };
    };
  };
  system.stateVersion = "24.05";
}
