# modules/nixos/bundles/common/default.nix
{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.bundles.common;
in {
  options.${namespace}.bundles.common = with types; {
    enable = mkBoolOpt false "Whether or not to enable common configuration.";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
    ];

    ${namespace} = {
      config = {
        nix = mkDefault enabled;
      };

      hardware = {
        networking = mkDefault enabled;
      };
    };
  };
}
