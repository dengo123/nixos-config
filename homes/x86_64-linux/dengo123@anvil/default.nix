{
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
      desktop.hyprland = enabled;
      development = enabled;
      office = enabled;
    };

    desktop.hyprland = {
      enable = true;
      monitor.mode = "vert-1";
      plugins = {
        split-monitor = enabled;
      };
    };

    programs = {
      git = {
        enable = true;
        username = "dengo123";
      };
      nixvim = {
        enable = true;
        mode = "full";
      };
      freetube = enabled;
      spotify = enabled;
    };
    misc = {
      xdg = enabled;
    };
  };

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "24.05";
}
