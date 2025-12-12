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
      development = enabled;
      shell.mode = "emacs"; # full/emacs/lite
      terminal = disabled;
      # terminal.emulator = "kitty";
      # browser.app = "zen";

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
      freetube = enabled;
      spotify = enabled;
      gimp = enabled;
      discord = enabled;
      doom = {
        enable = true;
        doomDir = inputs."doom-config";
        emacs = pkgs.emacs;
      };
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
  home.sessionVariables = {
    EDITOR = "emacs";
    VISUAL = "emacsclient -c -a emacs";
    # BROWSER = "zen";
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
