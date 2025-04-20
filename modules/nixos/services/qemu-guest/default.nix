{
  lib,
  config,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; {
  options.${namespace}.services.qemu-guest.enable =
    mkBoolOpt false "Enable QEMU guest integration.";

  config = mkIf config.${namespace}.services.qemu-guest.enable {
    services.qemuGuest.enable = true;
  };
}
