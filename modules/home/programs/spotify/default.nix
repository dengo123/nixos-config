{
  pkgs,
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
    programs.spotify-player = {
      enable = true;
      settings = {
        client_id = "042e17bf925744edb90d80b4b045ceb7";
        login_redirect_uri = "http://127.0.0.1:8989/login";
      };
    };
    home.packages = with pkgs; [cava];
  };
}
