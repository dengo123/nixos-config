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
  anyEnabled =
    (config.${namespace}.programs.brave.enable or false)
    || (config.${namespace}.programs.librewolf.enable or false)
    || (config.${namespace}.programs.zen.enable or false);
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
    # Gewählten Browser einschalten (deine bestehenden Module übernehmen die Konfig)
    (mkIf (cfg.app == "brave") {${namespace}.programs.brave.enable = true;})
    (mkIf (cfg.app == "librewolf") {${namespace}.programs.librewolf.enable = true;})
    (mkIf (cfg.app == "zen") {${namespace}.programs.zen.enable = true;})

    # Fallback: nichts gewählt und kein anderer Browser aktiviert → Firefox
    (mkIf (cfg.app == null && !anyEnabled) {
      programs.firefox.enable = true;
    })
  ]);
}
