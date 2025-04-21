{
  config,
  lib,
  osConfig,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; {
  snowfallorg.user.enable = true;

  nixforge = {
    bundles = {
      common = enabled;
      # Add optional bundles below
      # desktop.hyprland = enabled;
      # development = enabled;
      # office = enabled;
    };

    programs = {
      git = {
        enable = true;
        username = "your-username"; # TODO: Change me!
      };
      nixvim = {
        enable = true;
        mode = "devshell";
      };
    };

    misc = {
      xdg = enabled;
    };
  };

  home.stateVersion = lib.mkDefault (osConfig.system.stateVersion or "24.05");
}
