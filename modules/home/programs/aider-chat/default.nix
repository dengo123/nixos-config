# modules/home/programs/aider-chat/default.nix
{
  config,
  lib,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.programs.aider-chat;
in {
  options.${namespace}.programs.aider-chat = with types; {
    enable = mkBoolOpt false "Enable aider chat";
  };

  config = mkIf cfg.enable {
    programs.aider-chat = {
      enable = true;
      settings = {
        model = "ollama/devstral-aider";
        auto-commits = false;
        dirty-commits = false;
        show-diffs = true;
        stream = true;
        map-tokens = 4096;
      };
    };
  };
}
