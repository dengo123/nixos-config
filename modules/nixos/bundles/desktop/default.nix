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
      desktop = {
        awesome = {
          enable = mkDefault true;
          package = mkDefault "patched";
        };

        lightdm.enable = mkDefault true;
      };

      hardware = {
        audio = mkDefault enabled;
        bluetooth = mkDefault enabled;
      };

      services = {
        printing = mkDefault enabled;
        udisks2 = mkDefault enabled;
        wireplumber.enable = mkDefault true;
      };

      programs = {
        screenshot = mkDefault enabled;
      };

      system = {
        networking.networkmanager = {
          enable = mkDefault true;
        };

        fonts.enable = mkDefault true;
        cursor.enable = mkDefault true;
      };
    };
  };
}
