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
      stow
    ];
    ${namespace} = {
      config = {
        nix = enabled;
      };

      hardware = {
        audio = enabled;
        networking = enabled;
        bluetooth = enabled;
      };

      services = {
        security = enabled;
        printing = enabled;
        tailscale = enabled;
        udisks2 = enabled;
      };

      system = {
        fonts = {
          fontconfig = disabled;
          packages = with pkgs; [];
        };
      };
    };
  };
}
