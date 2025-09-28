{
  lib,
  config,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.programs.wezterm;
in {
  options.${namespace}.programs.wezterm = with types; {
    enable = mkBoolOpt false "Enable Wezterm terminal emulator.";
  };

  config = mkIf cfg.enable {
    programs.wezterm.enable = true;
  };
}
