# modules/nixos/hardware/monitors/default.nix
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

  dgpuScript = ''
    #!/bin/sh
    set -u

    # optional: mini delay für EDID/CRTC Stabilität (kannst du auch auf 0 setzen)
    ${optionalString (cfg.delayMs > 0) "sleep ${toString (cfg.delayMs)}e-3"}

    # Portrait zuerst (Rotation/CRTC), dann Primary
    ${XR} --output DP-2 --mode 1920x1080 --rotate left --pos 0x0 2>/dev/null || true
    ${XR} --output DP-4 --mode 1920x1080 --rotate normal --primary --pos 1080x420 2>/dev/null || true

    # TV nur wenn wirklich connected
    if ${XR} --query | grep -q "^HDMI-0 connected"; then
      ${XR} --output HDMI-0 --mode 3840x2160 --pos -3840x0 2>/dev/null || true
    fi

    # Best effort: andere dGPU outputs aus
    ${XR} --output DP-0 --off 2>/dev/null || true
    ${XR} --output DP-1 --off 2>/dev/null || true
    ${XR} --output DP-3 --off 2>/dev/null || true
    ${XR} --output DP-5 --off 2>/dev/null || true
  '';

  igpuScript = ''
    #!/bin/sh
    set -u

    ${optionalString (cfg.delayMs > 0) "sleep ${toString (cfg.delayMs)}e-3"}

    ${XR} --output HDMI-A-0       --mode 1920x1080 --rate 60 --rotate left   --pos 0x0      2>/dev/null || true
    ${XR} --output DisplayPort-0  --mode 1920x1080 --rate 60 --rotate normal --primary --pos 1080x420 2>/dev/null || true
  '';

  autoScript = ''
    #!/bin/sh
    set -u

    log() {
      echo "monitors-auto: $*" | systemd-cat -t monitors-auto 2>/dev/null || true
    }

    XR="/run/current-system/sw/bin/xrandr"

    # Wenn xrandr nicht da ist, nichts tun
    command -v "$XR" >/dev/null 2>&1 || exit 0

    q="$("$XR" --query 2>/dev/null || true)"

    if echo "$q" | grep -Eq '^DP-2 connected|^DP-4 connected'; then
      log "mode=dgpu (DP-2/DP-4 connected)"
      /etc/X11/monitors-dgpu.sh || true
    else
      log "mode=igpu (no DP-2/DP-4 connected)"
      /etc/X11/monitors-igpu.sh || true
    fi
  '';

  # Optional: CLI-Tools zum manuellen Ausführen (NICHT als Datei, sondern bin!)
  dgpuBin = pkgs.writeShellScriptBin "monitors-dgpu" dgpuScript;
  igpuBin = pkgs.writeShellScriptBin "monitors-igpu" igpuScript;
  autoBin = pkgs.writeShellScriptBin "monitors-auto" autoScript;

in
{
  options.${namespace}.hardware.monitors = with types; {
    enable = mkBoolOpt false "Enable early X11 monitor init via /etc Xrandr scripts (dGPU/iGPU auto-detect).";

    # wenn du die scripts auch im terminal haben willst:
    installTools = mkBoolOpt true "Install monitors-* commands system-wide for debugging/manual use.";

    # kleiner delay kann bei manchen Setups helfen (0 = aus)
    delayMs = mkIntOpt 0 "Delay in milliseconds before applying xrandr (helps with EDID/CRTC races).";
  };

  config = mkIf cfg.enable {
    # /etc/X11 scripts (DM-safe, immer verfügbar)
    environment.etc."X11/monitors-dgpu.sh".text = dgpuScript;
    environment.etc."X11/monitors-dgpu.sh".mode = "0755";

    environment.etc."X11/monitors-igpu.sh".text = igpuScript;
    environment.etc."X11/monitors-igpu.sh".mode = "0755";

    environment.etc."X11/monitors-auto.sh".text = autoScript;
    environment.etc."X11/monitors-auto.sh".mode = "0755";

    # SDDM/DM hook: vor Greeter & Session
    services.xserver.displayManager.setupCommands = ''
      /etc/X11/monitors-auto.sh || true
    '';

    services.xserver.displayManager.sessionCommands = ''
      /etc/X11/monitors-auto.sh || true
    '';

    # Optional: Debug/Manual Tools (bins, kein buildEnv-file-crash)
    environment.systemPackages = mkIf cfg.installTools [
      dgpuBin
      igpuBin
      autoBin
      pkgs.xorg.xrandr
    ];
  };
}
