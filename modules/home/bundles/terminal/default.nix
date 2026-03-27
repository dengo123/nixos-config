# modules/home/bundles/terminal/default.nix
{
  lib,
  config,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.bundles.terminal;
in {
  options.${namespace}.bundles.terminal = with types; {
    enable = mkBoolOpt false "Enable the terminal bundle.";

    emulator =
      mkOpt (types.nullOr (
        types.enum [
          "ghostty"
          "kitty"
          "wezterm"
        ]
      ))
      null
      "Choose a terminal emulator. If null, no terminal emulator is enabled by this bundle.";
  };

  config = mkIf cfg.enable (mkMerge [
    (mkIf (cfg.emulator == "ghostty") {
      ${namespace}.programs.ghostty.enable = true;
    })

    (mkIf (cfg.emulator == "kitty") {
      ${namespace}.programs.kitty.enable = true;
    })

    (mkIf (cfg.emulator == "wezterm") {
      ${namespace}.programs.wezterm.enable = true;
    })
  ]);
}
