# homes/x86_64-linux/dengo123@anvil/default.nix
{
  inputs,
  pkgs,
  lib,
  namespace,
  ...
}:
with lib;
with lib.${namespace};
{
  snowfallorg.user.enable = true;

  nixforge = {
    bundles = {
      common = enabled;
      office = enabled;
      ide.editor = "doom";

      desktop = {
        awesome = enabled;
        hyprland = disabled;
      };
    };

    programs = {
      git = {
        enable = true;
        username = "dengo123";
      };
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
    };
  };
  home.stateVersion = "24.05";
}
