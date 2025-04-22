{
  config,
  lib,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.programs.lazygit;
  accent = "#${config.lib.stylix.colors.base0D}";
  muted = "#${config.lib.stylix.colors.base03}";
in {
  options.${namespace}.programs.lazygit = with types; {
    enable = mkBoolOpt false "Enable programs.lazygit";
  };

  config = mkIf cfg.enable {
    programs.lazygit = {
      enable = true;
      settings = lib.mkForce {
        gui = {
          theme = {
            activeBorderColor = [
              accent
              "bold"
            ];
            inactiveBorderColor = [muted];
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
