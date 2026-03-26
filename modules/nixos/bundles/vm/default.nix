# modules/nixos/bundles/vm/default.nix
{
  config,
  lib,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.bundles.vm;
in {
  options.${namespace}.bundles.vm = with types; {
    enable = mkBoolOpt false "Whether or not to enable virtual machine guest integration.";
  };

  config = mkIf cfg.enable {
    ${namespace} = {
      services = {
        qemu-guest = mkDefault enabled;
        spice-agent = mkDefault enabled;
        virtiofsd = mkDefault disabled;
      };
    };
  };
}
