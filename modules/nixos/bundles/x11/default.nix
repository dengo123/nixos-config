{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.bundles.x11;
in {
  options.${namespace}.bundles.x11 = with types; {
    enable = mkBoolOpt false "Whether or not to enable common configuration.";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      pavucontrol
    ];
    ${namespace} = {
      desktop.xsession = enabled;

      programs = {
        screenshot = enabled;
        nemo = enabled;
      };
    };
  };
}
