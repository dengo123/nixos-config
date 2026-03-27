# modules/home/misc/xdg/default.nix
{
  config,
  lib,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.misc.xdg;
in {
  options.${namespace}.misc.xdg = with types; {
    enable = mkBoolOpt false "Enable XDG support.";

    mime = {
      enable = mkBoolOpt false "Enable selected MIME application associations.";
      defaultApplications = mkOpt (attrsOf (listOf str)) {} "Default application associations.";
    };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      xdg.enable = true;
    }

    (mkIf cfg.mime.enable {
      xdg.mimeApps = {
        enable = true;
        defaultApplications = cfg.mime.defaultApplications;
      };
    })
  ]);
}
