# modules/nixos/system/cursor/default.nix
{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.system.cursor;
in {
  options.${namespace}.system.cursor = with types; {
    enable = mkBoolOpt false "Enable system-wide cursor configuration.";

    package = mkOpt package pkgs.bibata-cursors "Cursor theme package.";
    name = mkOpt str "Bibata-Original-Ice" "Default cursor theme name.";
    size = mkOpt int 24 "Default cursor size.";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [
      cfg.package
    ];

    environment.sessionVariables = {
      XCURSOR_THEME = cfg.name;
      XCURSOR_SIZE = toString cfg.size;
    };
  };
}
