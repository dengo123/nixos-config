# modules/nixos/hardware/xmonitors/default.nix
{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.hardware.xmonitors;

  XR = "${pkgs.xorg.xrandr}/bin/xrandr";

  xsetup = pkgs.writeShellScript "dm-xsetup" ''
    #!/usr/bin/env bash
    set -euo pipefail

    export DISPLAY=":0"
    export PATH=${
      lib.makeBinPath [
        pkgs.coreutils
        pkgs.gnugrep
        pkgs.gawk
        pkgs.util-linux
        pkgs.xorg.xrandr
      ]
    }

    log() { echo "dm-xsetup: $*" >&2; }

    connected() {
      ${XR} --query | grep -q "^$1 connected"
    }

    # Wait until X enumerated outputs
    for i in $(seq 1 30); do
      if ${XR} --query >/dev/null 2>&1 && ${XR} --query | grep -q " connected"; then
        break
      fi
      sleep 0.2
    done

    # Big framebuffer to avoid "not large enough"
    ${XR} --fb 8192x4320 || true

    # --- iGPU layout ---
    if connected "DisplayPort-0" || connected "DisplayPort-0"; then
      log "detected iGPU connectors -> applying iGPU layout"

      if connected "DisplayPort-0"; then
        ${XR} --output DisplayPort-0 --mode 1920x1080 --rate 60 --pos 0x0 --rotate left || true
      fi

      if connected "HDMI-A-0"; then
        ${XR} --output HDMI-A-0 --primary --mode 1920x1080 --rate 60 --pos 1080x420 --rotate normal || true
      fi

      # best effort disable dGPU names if present
      ${XR} --output DP-2 --off 2>/dev/null || true
      ${XR} --output DP-0 --off 2>/dev/null || true
      ${XR} --output HDMI-0 --off 2>/dev/null || true
      exit 0
    fi

    # --- dGPU layout ---
    if connected "DP-0" || connected "DP-2" || connected "HDMI-0"; then
      log "detected dGPU connectors -> applying dGPU-work baseline (HDMI off)"

      ${XR} --fb 8192x4320 || true

      if connected "DP-2"; then
        ${XR} --output DP-2 --mode 1920x1080 --rate 60 --pos 0x0 --rotate left || true
      fi

      if connected "DP-0"; then
        ${XR} --output DP-0 --primary --mode 1920x1080 --rate 60 --pos 1080x420 --rotate normal || true
      fi

      # HDMI default OFF
      if connected "HDMI-0"; then
        ${XR} --output HDMI-0 --off 2>/dev/null || true
      fi

      exit 0
    fi

    log "no known connector set found; leaving defaults"
    exit 0
  '';
in {
  options.${namespace}.hardware.xmonitors = {
    enable = mkBoolOpt false "Apply X11 monitor layout via xrandr in displayManager.setupCommands (DM-agnostic).";
    script = mkOpt types.package xsetup "The setup script to run (defaults to built-in xrandr layout).";
  };

  config = mkIf cfg.enable {
    services.xserver.enable = mkDefault true;

    services.xserver.displayManager.setupCommands = mkAfter ''
      ${cfg.script}
    '';
  };
}
