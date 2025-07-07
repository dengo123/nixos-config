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
    boot.loader = {
      efi.canTouchEfiVariables = true;

      grub = {
        enable = true;
        device = "nodev";
        efiSupport = true;
        useOSProber = false;
      };

      timeout = 5;
    };
  };
}
