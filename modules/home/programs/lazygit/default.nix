{ config, lib, namespace, ... }:
with lib;
with lib.${namespace};

let
  cfg = config.${namespace}.programs.lazygit;
in
{
  options.${namespace}.programs.lazygit = with types; {
    enable = mkBoolOpt false "Enable programs.lazygit";
  };

  config = mkIf cfg.enable {
    programs.lazygit = {
      enable = true;
      settings = lib.mkForce {
        gui = {
          theme = {
            activeBorderColor = [ "blue" "bold" ];
            inactiveBorderColor = [ "grey" ];
          };
          showListFooter = false;
          showRandomTip = false;
          showCommandLog = false;
          showBottomLine = false;
          nerdFontsVersion = "3";
        };
      };
    };
  };
}

