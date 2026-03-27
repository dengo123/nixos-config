# homes/x86_64-linux/dengo123@anvil/default.nix
{
  inputs,
  pkgs,
  lib,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; {
  snowfallorg.user.enable = true;

  nixforge = {
    bundles = {
      terminal.emulator = "kitty";
      browser.app = "firefox";
      files.manager = "nemo";
      developer.editor = "doom";
      office = enabled;
    };

    programs = {
      # starship.template = "default";
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
    };
  };
  home.stateVersion = "24.05";
}
