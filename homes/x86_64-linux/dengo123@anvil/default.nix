# homes/x86_64-linux/dengo123@anvil/default.nix
{
  inputs,
  lib,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; {
  snowfallorg.user.enable = true;

  nixforge = {
    bundles = {
      desktop = enabled;
      terminal.emulator = "kitty";
      browser.app = "firefox";
      files.manager = "nemo";
      developer.editor = "doom";
      office = enabled;
    };

    programs = {
      starship.template = "powerline_rainbow";
      # nemo.withBundle = "true";
      spotify = enabled;
      gimp = enabled;
      discord = enabled;
    };
    misc = {
      gtk = {
        iconTheme = "Papirus-Dark"; # or Papirus-light"

        # cursor = {
        #   package = pkgs.bibata-cursors;
        #   name = "Bibata-Modern-Ice";
        #   size = 26;
        # };
      };
    };

    services = {
      picom = {
        configFile = mkDefault (inputs.self + /dotfiles/picom/.config/picom/picom.conf);
        manageConfig = false;
      };

      redshift = {
        provider = "manual";
        latitude = 50.1;
        longitude = 8.6;
      };

      xidlehook = {
        # notWhenAudio = true;
        timers = [
          {
            delay = 300;
            command = "sh -c 'date >> /tmp/xidlehook-600.log'";
            canceller = "";
          }
          {
            delay = 1200;
            command = "xset dpms force off";
            canceller = "";
          }
          {
            delay = 1800;
            command = "systemctl suspend";
            canceller = "";
          }
        ];
      };
    };
  };
  home.stateVersion = "24.05";
}
