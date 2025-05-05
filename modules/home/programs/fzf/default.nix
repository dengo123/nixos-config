{ config, lib, namespace, ... }:
with lib;
with lib.${namespace};

let
  cfg = config.${namespace}.programs.fzf;
in
{
  options.${namespace}.programs.fzf = with types; {
    enable = mkBoolOpt false "Enable programs.fzf";
  };

  config = mkIf cfg.enable {
    programs.fzf = {
      enable = true;
      enableZshIntegration = true;
    };
  };
}

