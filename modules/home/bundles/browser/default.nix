# modules/home/bundles/browser/default.nix
{
  lib,
  config,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.bundles.browser;
in {
  options.${namespace}.bundles.browser = {
    enable = mkBoolOpt true "Enable the browser bundle.";
    app =
      mkOpt (types.nullOr (
        types.enum [
          "brave"
          "librewolf"
          "zen"
        ]
      ))
      null "Choose a browser app. If null, Firefox is installed as fallback.";
  };

  config = mkIf cfg.enable (mkMerge [
    (mkIf (cfg.app == "brave") {${namespace}.programs.brave.enable = true;})
    (mkIf (cfg.app == "librewolf") {${namespace}.programs.librewolf.enable = true;})
    (mkIf (cfg.app == "zen") {${namespace}.programs.zen.enable = true;})

    (mkIf (cfg.app == null) {
      home.packages = [pkgs.firefox];
    })

    {${namespace}.config.apps = enabled;}
  ]);
}
