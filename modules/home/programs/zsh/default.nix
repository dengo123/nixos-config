{
  lib,
  config,
  namespace,
  ...
}:
with lib;
with lib.${namespace};
let
  cfg = config.${namespace}.programs.zsh;
  aliases = import ./config/aliases.nix;
  syntaxStyles = import ./config/syntax-style.nix;
in
{
  options.${namespace}.programs.zsh.enable = mkBoolOpt false "Enable Zsh";

  config = mkIf cfg.enable {
    programs.zsh = {
      enable = true;
      enableCompletion = true;
      autosuggestion = {
        enable = true;
        highlight = "fg=green";
      };
      defaultKeymap = "emacs";
      historySubstringSearch.enable = true;
      syntaxHighlighting = {
        enable = true;
        styles = syntaxStyles;
      };
      shellAliases = aliases;
    };
  };
}
