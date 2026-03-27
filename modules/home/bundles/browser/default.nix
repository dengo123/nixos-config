# modules/home/bundles/browser/default.nix
{
  lib,
  config,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.bundles.browser;
in {
  options.${namespace}.bundles.browser = with types; {
    enable = mkBoolOpt false "Enable the browser bundle.";

    app =
      mkOpt (types.nullOr (
        types.enum [
          "firefox"
          "brave"
          "librewolf"
          "zen"
        ]
      ))
      null
      "Choose a browser app. If null, no browser is enabled by this bundle.";
  };

  config = mkIf cfg.enable (mkMerge [
    (mkIf (cfg.app == "firefox") {
      ${namespace}.programs.firefox.enable = true;
    })

    (mkIf (cfg.app == "brave") {
      ${namespace}.programs.brave.enable = true;
    })

    (mkIf (cfg.app == "librewolf") {
      ${namespace}.programs.librewolf.enable = true;
    })

    (mkIf (cfg.app == "zen") {
      ${namespace}.programs.zen.enable = true;
    })
  ]);
}
