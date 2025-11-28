# modules/home/bundles/terminal/default.nix
{
  lib,
  config,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.bundles.terminal;
in {
  options.${namespace}.bundles.terminal = {
    enable = mkBoolOpt true "Enable the terminal bundle.";
    emulator =
      mkOpt (types.nullOr (
        types.enum [
          "kitty"
          "wezterm"
          "ghostty"
        ]
      ))
      null "Choose a terminal emulator. If null, xterm is installed as fallback.";
  };

  config = mkIf cfg.enable (mkMerge [
    (mkIf (cfg.emulator == "kitty") {${namespace}.programs.kitty.enable = true;})
    (mkIf (cfg.emulator == "wezterm") {${namespace}.programs.wezterm.enable = true;})
    (mkIf (cfg.emulator == "ghostty") {${namespace}.programs.ghostty.enable = true;})

    (mkIf (cfg.emulator == null) {
      home.packages = [pkgs.xterm];
    })

  ]);
}
