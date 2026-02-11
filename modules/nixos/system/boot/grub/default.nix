# module/nixos/system/boot/grub/default.nix
{
  config,
  lib,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.system.boot.grub;
in {
  options.${namespace}.system.boot.grub = with types; {
    enable = mkBoolOpt false "Whether or not to enable grub booting.";
  };

  config = mkIf cfg.enable {
    boot = {
      loader = {
        efi.canTouchEfiVariables = true;

        grub = {
          enable = true;
          device = "nodev";
          efiSupport = true;
          useOSProber = false;
        };

        timeout = 10;
      };
      kernelParams = [
        "resume=UUID=5c987df5-d144-43ae-9db1-899a7d6f5424"
      ];
    };
  };
}
