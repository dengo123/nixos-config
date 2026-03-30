# modules/home/programs/firefox/default.nix
{
  config,
  lib,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.programs.firefox;
in {
  options.${namespace}.programs.firefox = with types; {
    enable = mkBoolOpt false "Enable Firefox.";
  };

  config = mkIf cfg.enable {
    programs.firefox = {
      enable = true;
    };

    home.sessionVariables.BROWSER = "firefox";
  };
}
