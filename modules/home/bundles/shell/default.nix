{
  config,
  lib,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.bundles.shell;
in {
  options.${namespace}.bundles.shell = with types; {
    enable = mkBoolOpt false "Enable shell bundle";
  };

  config = mkIf cfg.enable {
    nixforge = {
      programs = {
        atuin = enabled;
        eza = enabled;
        fzf = enabled;
        powerlevel10k = disabled;
        starship = disabled;
        yazi = enabled;
        zoxide = enabled;
        zsh = enabled;
      };
    };
  };
}
