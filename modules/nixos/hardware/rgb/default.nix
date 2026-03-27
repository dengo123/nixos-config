# modules/nixos/hardware/rgb/default.nix
{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.hardware.rgb;

  userName = config.${namespace}.config.user.name;
  userHome = "/home/${userName}";

  openrgbPkg = pkgs.openrgb;
  profilePath = "${userHome}/.config/OpenRGB/${cfg.profile}";

  applyProfileScript = pkgs.writeShellScript "openrgb-apply-profile" ''
    set -euo pipefail

    export HOME="${userHome}"
    export XDG_CONFIG_HOME="${userHome}/.config"

    ${pkgs.kmod}/bin/modprobe i2c-dev 2>/dev/null || true
    ${pkgs.kmod}/bin/modprobe i2c-piix4 2>/dev/null || true
    ${pkgs.systemd}/bin/udevadm settle --timeout=30 || true

    ${openrgbPkg}/bin/openrgb --noautoconnect --profile "${profilePath}" || true
  '';

  disableRgbScript = pkgs.writeShellScript "openrgb-disable-rgb" ''
    set -euo pipefail

    ${pkgs.kmod}/bin/modprobe i2c-dev 2>/dev/null || true
    ${pkgs.kmod}/bin/modprobe i2c-piix4 2>/dev/null || true
    ${pkgs.systemd}/bin/udevadm settle --timeout=30 || true

    num_devices="$(${openrgbPkg}/bin/openrgb --noautoconnect --list-devices | ${pkgs.gnugrep}/bin/grep -E '^[0-9]+: ' | ${pkgs.coreutils}/bin/wc -l)"

    if [ "$num_devices" -eq 0 ]; then
      exit 0
    fi

    for i in $(${pkgs.coreutils}/bin/seq 0 "$((num_devices - 1))"); do
      ${openrgbPkg}/bin/openrgb --noautoconnect --device "$i" --mode static --color 000000 || true
    done
  '';

  resumeScript = pkgs.writeShellScript "openrgb-resume" ''
    set -euo pipefail

    export HOME="${userHome}"
    export XDG_CONFIG_HOME="${userHome}/.config"

    ${pkgs.kmod}/bin/modprobe i2c-dev 2>/dev/null || true
    ${pkgs.kmod}/bin/modprobe i2c-piix4 2>/dev/null || true
    ${pkgs.systemd}/bin/udevadm settle --timeout=30 || true

    ${pkgs.coreutils}/bin/sleep 10

    if [ "${cfg.mode}" = "profile" ]; then
      ${openrgbPkg}/bin/openrgb --noautoconnect --profile "${profilePath}" || true
    else
      num_devices="$(${openrgbPkg}/bin/openrgb --noautoconnect --list-devices | ${pkgs.gnugrep}/bin/grep -E '^[0-9]+: ' | ${pkgs.coreutils}/bin/wc -l)"

      if [ "$num_devices" -gt 0 ]; then
        for i in $(${pkgs.coreutils}/bin/seq 0 "$((num_devices - 1))"); do
          ${openrgbPkg}/bin/openrgb --noautoconnect --device "$i" --mode static --color 000000 || true
        done
      fi
    fi
  '';
in {
  options.${namespace}.hardware.rgb = with types; {
    enable = mkBoolOpt false "Manage RGB via OpenRGB.";

    mode = mkOpt (enum [
      "profile"
      "off"
    ]) "profile" "Whether to apply an OpenRGB profile or switch RGB off.";

    profile = mkOpt (nullOr str) null "Profile filename in ~/.config/OpenRGB (for mode = profile).";

    applyOnResume = mkBoolOpt true "Re-apply RGB state after resume.";
  };

  config = mkIf cfg.enable (mkMerge [
    {
      assertions = [
        {
          assertion = cfg.mode != "profile" || cfg.profile != null;
          message = "${namespace}.hardware.rgb.profile must be set when mode = \"profile\".";
        }
      ];

      environment.systemPackages = [openrgbPkg];
      services.udev.packages = [openrgbPkg];

      hardware.i2c.enable = true;

      boot.kernelModules = [
        "i2c-dev"
        "i2c-piix4"
      ];
    }

    (mkIf (cfg.mode == "profile") {
      systemd.user.services.openrgb-apply = {
        description = "OpenRGB apply profile on login";
        wantedBy = ["default.target"];
        after = ["graphical-session.target"];
        serviceConfig = {
          Type = "oneshot";
          ExecStart = applyProfileScript;
        };
      };
    })

    (mkIf (cfg.mode == "off") {
      systemd.services.openrgb-disable = {
        description = "Disable RGB via OpenRGB";
        wantedBy = ["multi-user.target"];
        after = ["systemd-udev-settle.service"];
        wants = ["systemd-udev-settle.service"];
        serviceConfig = {
          Type = "oneshot";
          ExecStart = disableRgbScript;
        };
      };
    })

    (mkIf cfg.applyOnResume {
      systemd.services.openrgb-resume = {
        description = "Restore OpenRGB state after resume";
        wantedBy = [
          "suspend.target"
          "hibernate.target"
        ];
        after = [
          "suspend.target"
          "hibernate.target"
          "systemd-udev-settle.service"
        ];
        wants = ["systemd-udev-settle.service"];
        serviceConfig = {
          Type = "oneshot";
          ExecStart = resumeScript;
          StandardOutput = "journal";
          StandardError = "journal";
        };
      };
    })
  ]);
}
