# modules/bundles/security/default.nix
{
  config,
  lib,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.bundles.security;
in {
  options.${namespace}.bundles.security = with types; {
    enable = mkBoolOpt false "Whether or not to enable security hardening.";
  };

  config = mkIf cfg.enable {
    ${namespace} = {
      services = {
        apparmor = mkDefault enabled;
        polkit = mkDefault enabled;
      };

      system = {
        filesystem = mkDefault enabled;
        sudo = mkDefault enabled;
        systemd = mkDefault enabled;
      };
    };
  };
}
