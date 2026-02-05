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
  cfg = config.${namespace}.hardware.monitors;

  XR = "${pkgs.xorg.xrandr}/bin/xrandr";

  # ---- Helpers: Erkennung über /sys (zuverlässiger als lspci im DM-Kontext) ----
  detectGpuScript = pkgs.writeShellScript "detect-gpu-mode" ''
    #!/usr/bin/env bash
    set -u

    # NVIDIA vendor id: 0x10de
    if [ -e /sys/class/drm/card2/device/vendor ] && grep -qi 0x10de /sys/class/drm/card2/device/vendor; then
      echo dgpu
      exit 0
    fi

    # fallback: irgendeine card mit 0x10de?
    for v in /sys/class/drm/card*/device/vendor; do
      if [ -e "$v" ] && grep -qi 0x10de "$v"; then
        echo dgpu
        exit 0
      fi
    done

    echo igpu
  '';

  # ---- dGPU Layout (deine bekannten Ports) ----
  dgpuBin = pkgs.writeShellScriptBin "monitors-dgpu" ''
    #!/usr/bin/env bash
    set -u

    # Best effort: niemals den DM hart abschießen
    ${XR} --output DP-2 --mode 1920x1080 --rotate left --pos 0x0 2>/dev/null || true
    ${XR} --output DP-4 --mode 1920x1080 --primary --pos 1080x420 2>/dev/null || true

    # TV optional (wenn du ihn dran hast)
    ${XR} --output HDMI-0 --mode 3840x2160 --pos -3840x0 2>/dev/null || true

    # andere dGPU Outputs aus (nur dGPU-Namensraum!)
    ${XR} --output DP-0 --off 2>/dev/null || true
    ${XR} --output DP-1 --off 2>/dev/null || true
    ${XR} --output DP-3 --off 2>/dev/null || true
    ${XR} --output DP-5 --off 2>/dev/null || true
  '';

  # ---- iGPU Layout (aus deinem TTY-Screenshot: HDMI-A-1 + DP-1) ----
  igpuBin = pkgs.writeShellScriptBin "monitors-igpu" ''
    #!/usr/bin/env bash
    set -u

    ${XR} --output HDMI-A-1 --mode 1920x1080 --primary --pos 0x0 2>/dev/null || true
    ${XR} --output DP-1     --mode 1920x1080 --pos 1920x0 2>/dev/null || true

    # iGPU-only: NICHT versuchen HDMI-0/DP-2/... abzuschalten,
    # weil diese Namen auf iGPU-boot evtl. gar nicht existieren und nur Noise erzeugen.
  '';

  autoBin = pkgs.writeShellScriptBin "monitors-auto" ''
    #!/usr/bin/env bash
    set -u

    mode="$(${detectGpuScript})"

    # kleines Log, super hilfreich wenn DM hängt:
    echo "monitors-auto: mode=$mode" | systemd-cat -t monitors-auto || true

    if [ "$mode" = "dgpu" ]; then
      exec ${dgpuBin}/bin/monitors-dgpu
    else
      exec ${igpuBin}/bin/monitors-igpu
    fi
  '';
in
{
  options.${namespace}.hardware.monitors = with types; {
    enable = mkBoolOpt false "Enable early X11 monitor init via xrandr (dGPU/iGPU auto-detect).";

    # optional: nur wenn du die Skripte auch manuell nutzen willst
    installTools = mkBoolOpt true "Install monitors-* scripts system-wide.";
  };

  config = mkIf cfg.enable {
    # Skripte optional ins System (praktisch für Debug)
    environment.systemPackages = mkIf cfg.installTools [
      dgpuBin
      igpuBin
      autoBin
    ];

    # Der wichtigste Hook: vor Greeter/Session
    services.xserver.displayManager.setupCommands = ''
      # niemals hart failen im DM-Startup
      ${autoBin}/bin/monitors-auto || true
    '';
  };
}
