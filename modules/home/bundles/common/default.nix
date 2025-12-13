{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace};
let
  cfg = config.${namespace}.bundles.common;
in
{
  options.${namespace}.bundles.common = with types; {
    enable = mkBoolOpt false "Whether or not to enable common bundle configuration.";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
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

      bitwarden-desktop

      # Config formatting
      nixfmt-rfc-style
    ];
    nixforge = {
      misc = {
        scripts = enabled;
      };
    };
  };
}
