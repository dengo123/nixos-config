# modules/nixos/hardware/rgb/default.nix
{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace};
let
  cfg = config.${namespace}.hardware.rgb;

  userName = config.${namespace}.config.user.name;
  userHome = "/home/${userName}";

  openrgbPkg = pkgs.openrgb-with-all-plugins;
  profilePath = "${userHome}/.config/OpenRGB/${cfg.profile}";

  resumeScript = pkgs.writeShellScript "openrgb-resume" ''
    set -euo pipefail

    export HOME="${userHome}"
    export XDG_CONFIG_HOME="${userHome}/.config"
    export PATH="${pkgs.coreutils}/bin:${pkgs.kmod}/bin:/run/current-system/sw/bin"

    # SMBus/i2c wieder sicherstellen
    ${pkgs.kmod}/bin/modprobe i2c-dev 2>/dev/null || true
    ${pkgs.kmod}/bin/modprobe i2c-piix4 2>/dev/null || true

    # warten bis udev durch ist (devices kommen nach hibernate gern spät)
    ${pkgs.systemd}/bin/udevadm settle --timeout=30 || true

    # Hibernate: RGB wird oft erst nach 30-60s final gesetzt -> wir kommen später
    ${pkgs.coreutils}/bin/sleep 60

    # Apply #1
    ${openrgbPkg}/bin/openrgb --noautoconnect --loglevel 6 \
      --profile "${profilePath}" || true

    # nochmal warten, falls Aura/DRAM ein zweites Mal resetten
    ${pkgs.coreutils}/bin/sleep 10

    # Apply #2 (der “killer” gegen spätes Rainbow)
    ${openrgbPkg}/bin/openrgb --noautoconnect --loglevel 6 \
      --profile "${profilePath}" || true

    exit 0
  '';
in
{
  options.${namespace}.hardware.rgb = with types; {
    enable = mkBoolOpt false "Apply an OpenRGB profile automatically (login + resume).";
    profile =
      mkOpt (nullOr str) (mkDefault null)
        "Profile filename in ~/.config/OpenRGB (e.g. all_white.orp).";
  };

  config = mkIf (cfg.enable && cfg.profile != null) {
    environment.systemPackages = [ openrgbPkg ];
    services.udev.packages = [ openrgbPkg ];

    boot.kernelModules = [
      "i2c-dev"
      "i2c-piix4"
    ];

    # Login apply (User)
    systemd.user.services.openrgb-apply = {
      description = "OpenRGB apply profile (login)";
      wantedBy = [ "default.target" ];
      after = [ "graphical-session.target" ];
      serviceConfig = {
        Type = "oneshot";
        Environment = [
          "HOME=${userHome}"
          "XDG_CONFIG_HOME=${userHome}/.config"
        ];
        ExecStart = "${openrgbPkg}/bin/openrgb --noautoconnect --profile ${profilePath}";
      };
    };

    # Resume apply (System)
    systemd.services.openrgb-resume = {
      description = "OpenRGB re-apply profile after resume";
      wantedBy = [
        "suspend.target"
        "hibernate.target"
      ];
      after = [
        "suspend.target"
        "hibernate.target"
        "systemd-udev-settle.service"
      ];
      wants = [ "systemd-udev-settle.service" ];

      serviceConfig = {
        Type = "oneshot";
        ExecStart = resumeScript;
        StandardOutput = "journal";
        StandardError = "journal";
      };
    };
  };
}
