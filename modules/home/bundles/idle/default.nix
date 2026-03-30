# modules/home/bundles/idle/default.nix
{
  config,
  lib,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.bundles.idle;
in {
  options.${namespace}.bundles.idle = with types; {
    enable = mkBoolOpt false "Enable idle-management configuration.";

    service = mkOpt (types.nullOr (types.enum [
      "xscreensaver"
      "xidlehook"
    ]))
    null "Choose the idle management service.";
  };

  config = mkIf cfg.enable (mkMerge [
    (mkIf (cfg.service == "xscreensaver") {
      ${namespace} = {
        services.xscreensaver.enable = true;
        misc.scripts.xscreensaver-idle.enable = true;
      };
    })

    (mkIf (cfg.service == "xidlehook") {
      ${namespace}.services.xidlehook.enable = true;
    })
  ]);
}
