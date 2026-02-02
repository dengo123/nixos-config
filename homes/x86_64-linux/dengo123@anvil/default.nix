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
      developer.editor = "doom";

      desktop = {
        awesome = enabled;
        # hyprland = enabled;
      };
    };

    programs = {
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
