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

    for _ in $(seq 1 10); do
      if ${openrgbPkg}/bin/openrgb --noautoconnect --profile "${profilePath}" >/dev/null 2>&1; then
        exit 0
      fi
      sleep 1
    done

    exit 0
  '';

  disableRgbScript = pkgs.writeShellScript "openrgb-disable-rgb" ''
    set -euo pipefail

    num_devices="$(${openrgbPkg}/bin/openrgb --noautoconnect --list-devices 2>/dev/null | ${pkgs.gnugrep}/bin/grep -E '^[0-9]+: ' | ${pkgs.coreutils}/bin/wc -l || true)"

    if [ -z "$num_devices" ] || [ "$num_devices" -eq 0 ]; then
      exit 0
    fi

    for i in $(${pkgs.coreutils}/bin/seq 0 "$((num_devices - 1))"); do
      ${openrgbPkg}/bin/openrgb --noautoconnect --device "$i" --mode static --color 000000 >/dev/null 2>&1 || true
    done
  '';

  resumeHook = pkgs.writeShellScript "openrgb-resume-hook" ''
    set -euo pipefail

    case "$1/$2" in
      post/*)
        (
          export HOME="${userHome}"
          export XDG_CONFIG_HOME="${userHome}/.config"
          export PATH=${lib.makeBinPath [
      pkgs.coreutils
      pkgs.gnugrep
      pkgs.kmod
      pkgs.systemd
      openrgbPkg
    ]}

          log_file="/tmp/openrgb-resume.log"

          {
            echo "==== $(date --iso-8601=seconds) openrgb resume start ($1/$2) ===="

            # Erst mal dem kaputten USB-Resume etwas Zeit geben
            sleep 8

            ${pkgs.kmod}/bin/modprobe i2c-dev 2>/dev/null || true
            ${pkgs.kmod}/bin/modprobe i2c-piix4 2>/dev/null || true
            ${pkgs.systemd}/bin/udevadm settle --timeout=10 || true

            for _ in $(seq 1 10); do
              if [ "${cfg.mode}" = "profile" ]; then
                echo "trying profile: ${profilePath}"
                if ${openrgbPkg}/bin/openrgb --noautoconnect --profile "${profilePath}"; then
                  echo "profile applied successfully"
                  exit 0
                fi
              else
                num_devices="$(${openrgbPkg}/bin/openrgb --noautoconnect --list-devices 2>/dev/null | ${pkgs.gnugrep}/bin/grep -E '^[0-9]+: ' | ${pkgs.coreutils}/bin/wc -l || true)"
                echo "devices detected: ''${num_devices:-0}"

                if [ -n "$num_devices" ] && [ "$num_devices" -gt 0 ]; then
                  for i in $(${pkgs.coreutils}/bin/seq 0 "$((num_devices - 1))"); do
                    ${openrgbPkg}/bin/openrgb --noautoconnect --device "$i" --mode static --color 000000 || true
                  done
                  echo "off mode applied"
                  exit 0
                fi
              fi

              echo "retrying in 2s..."
              sleep 2
            done

            echo "resume hook gave up after retries"
            exit 0
          } >> "$log_file" 2>&1
        ) &
        ;;
    esac
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
        before = ["getty@tty1.service" "display-manager.service"];
        serviceConfig = {
          Type = "oneshot";
          ExecStart = disableRgbScript;
        };
      };
    })

    (mkIf cfg.applyOnResume {
      environment.etc."systemd/system-sleep/90-openrgb-resume".source = resumeHook;
    })
  ]);
}
