# modules/nixos/bundles/gaming/default.nix
{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.bundles.gaming;
in {
  options.${namespace}.bundles.gaming = with types; {
    enable = mkBoolOpt false "Whether or not to enable gaming configuration.";
  };

  config = mkIf cfg.enable {
    ${namespace} = {
      programs = {
        steam = mkDefault enabled;
        prismlauncher = mkDefault enabled;
      };
    };

    environment.systemPackages = with pkgs; [
      protontricks
      lutris-unwrapped
      supertuxkart
    ];
  };
}
