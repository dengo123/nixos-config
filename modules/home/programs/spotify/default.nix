{
  config,
  lib,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.programs.spotify;
in {
  options.${namespace}.programs.spotify = with types; {
    enable = mkBoolOpt false "Enable a TUI spotify client";
  };

  config = mkIf cfg.enable {
    programs.spotify-player.enable = true;
  };
}
