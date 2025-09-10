{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.bundles.common;
in {
  options.${namespace}.bundles.common = with types; {
    enable = mkBoolOpt false "Whether or not to enable common bundle configuration.";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      # Terminal
      btop
      coreutils
      killall
      tldr
      wget

      # Video/Audio
      celluloid
      loupe

      # File Management
      unrar
      unzip
      zip

      bitwarden
      fastfetch

      # Config formatting
      nixfmt-rfc-style

      firefox
    ];
    nixforge = {
      bundles.shell = enabled;
      config = {
        apps = enabled;
      };
      misc = {
        gtk = disabled; # Done by stylix
        qt = disabled;
        scripts = enabled;
      };
      programs = {
        ghostty = enabled;
        lazygit = enabled;
        tmux = enabled;
        zen = enabled;
        stylix = enabled;
      };
    };
  };
}
