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
      ripgrep

      # Video/Audio
      celluloid
      loupe
      vlc

      # File Management
      unrar
      unzip
      zip
      appimage-run
      fuse
      libayatana-appindicator
      libappindicator-gtk3

      bitwarden
      fastfetch

      # Config formatting
      nixfmt-rfc-style

      firefox
    ];
    nixforge = {
      config = {
        apps = enabled;
      };
      misc = {
        scripts = enabled;
      };
    };
  };
}
