{
  lib,
  config,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.programs.ghostty;
in {
  options.${namespace}.programs.ghostty = with types; {
    enable = mkBoolOpt false "Enable Ghostty terminal emulator.";
  };

  config = mkIf cfg.enable {
    programs.ghostty.enable = true;
  };
}
