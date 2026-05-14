# modules/nixos/system/boot/default.nix
{
  config,
  lib,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.system.boot;

  quietKernelParams = [
    "quiet"
    "loglevel=0"
    "rd.systemd.show_status=false"
    "systemd.show_status=false"
    "udev.log_level=0"
  ];
in {
  options.${namespace}.system.boot = with types; {
    enable = mkBoolOpt true "Whether or not to enable bootloader configuration.";

    loader = mkOpt (enum [
      "grub"
      "systemd-boot"
      "generic-extlinux"
    ]) "systemd-boot" "Which bootloader to configure.";

    timeout = mkOpt int 5 "Bootloader timeout in seconds.";

    quietBoot = mkBoolOpt false "Whether or not to reduce visible boot console output.";

    efi = {
      canTouchEfiVariables = mkBoolOpt true "Whether the bootloader may modify EFI variables.";
    };

    extraKernelParams = mkOpt (listOf str) [] "Extra kernel parameters to append.";

    grub = {
      device = mkOpt str "nodev" "Device to install GRUB to.";
      efiSupport = mkBoolOpt true "Whether to enable EFI support for GRUB.";
      useOSProber = mkBoolOpt false "Whether GRUB should use os-prober.";
    };

    systemd-boot = {
      configurationLimit = mkOpt int 5 "Maximum number of boot entries to keep.";
      editor = mkBoolOpt false "Whether to allow editing boot entries at boot.";
    };
  };

  config = mkIf cfg.enable {
    boot = {
      loader = mkMerge [
        {
          timeout = cfg.timeout;
          efi.canTouchEfiVariables = cfg.efi.canTouchEfiVariables;
        }

        (mkIf (cfg.loader == "grub") {
          grub = {
            enable = true;
            device = cfg.grub.device;
            efiSupport = cfg.grub.efiSupport;
            useOSProber = cfg.grub.useOSProber;
          };

          systemd-boot.enable = false;
          generic-extlinux-compatible.enable = false;
        })

        (mkIf (cfg.loader == "systemd-boot") {
          systemd-boot = {
            enable = true;
            configurationLimit = cfg.systemd-boot.configurationLimit;
            editor = cfg.systemd-boot.editor;
          };

          grub.enable = false;
          generic-extlinux-compatible.enable = false;
        })

        (mkIf (cfg.loader == "generic-extlinux") {
          generic-extlinux-compatible.enable = true;

          grub.enable = false;
          systemd-boot.enable = false;
        })
      ];

      consoleLogLevel = mkIf cfg.quietBoot 0;
      initrd.verbose = !cfg.quietBoot;
      kernelParams =
        (optionals cfg.quietBoot quietKernelParams)
        ++ cfg.extraKernelParams;
    };
  };
}
