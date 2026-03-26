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
      common = enabled;
      office = enabled;
      developer.editor = "doom";
      terminal.emulator = "kitty";
    };

    programs = {
      # starship.template = "default";
      spotify = enabled;
      gimp = enabled;
      discord = enabled;
    };
    misc = {
      xdg = enabled;
      gtk = {
        iconTheme = "Papirus-Dark"; # or Papirus-Dark"

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
