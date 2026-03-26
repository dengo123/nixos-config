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
    };

    services = {
      polkit-agent = {
        enable = true;
        kind = "gnome";
      };
      picom.manageConfig = false;
    };
  };
  home.stateVersion = "24.05";
}
