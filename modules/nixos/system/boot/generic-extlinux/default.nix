# modules/nixos/system/boot/generic-extlinux/default.nix
{
  config,
  lib,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.system.boot.generic-extlinux;
in {
  options.${namespace}.system.boot.generic-extlinux = with types; {
    enable = mkBoolOpt false "Enable generic extlinux booting";
  };

  config = mkIf cfg.enable {
    boot.loader = {
      grub.enable = false;
      generic-extlinux-compatible.enable = true;
    };
  };
}
