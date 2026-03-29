# modules/nixos/bundles/desktop/default.nix
{
  config,
  lib,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.bundles.desktop;
in {
  options.${namespace}.bundles.desktop = with types; {
    enable = mkBoolOpt false "Whether or not to enable desktop configuration.";
  };

  config = mkIf cfg.enable {
    networking.networkmanager = mkDefault enabled;

    ${namespace} = {
      hardware = {
        audio = mkDefault enabled;
        bluetooth = mkDefault enabled;
      };

      services = {
        printing = mkDefault enabled;
        udisks2 = mkDefault enabled;
      };

      system = {
        fonts = {
          enable = mkDefault true;
          fontconfig = mkDefault enabled;
        };
      };
    };
  };
}
