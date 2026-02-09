# modules/nixos/desktop/awesome/default.nix
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
  cfg = config.${namespace}.desktop.awesome;
  userName = config.${namespace}.config.user.name or "dengo123";

  XR = "${pkgs.xorg.xrandr}/bin/xrandr";

  xsetup = pkgs.writeShellScript "sddm-xsetup" ''
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

    log() { echo "sddm-xsetup: $*" >&2; }

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

    # Big framebuffer to avoid "not large enough" when HDMI is 4K
    ${XR} --fb 8192x4320 || true

    # --- iGPU layout ---
    if connected "DisplayPort-0" || connected "HDMI-A-0"; then
      log "detected iGPU connectors -> applying iGPU layout"

      if connected "HDMI-A-0"; then
        ${XR} --output HDMI-A-0 --mode 1920x1080 --rate 60 --pos 0x0 --rotate left || true
      fi

      if connected "DisplayPort-0"; then
        ${XR} --output DisplayPort-0 --primary --mode 1920x1080 --rate 60 --pos 1080x420 --rotate normal || true
      fi

      # best effort disable dGPU names if present
      ${XR} --output DP-2 --off 2>/dev/null || true
      ${XR} --output DP-4 --off 2>/dev/null || true
      ${XR} --output HDMI-0 --off 2>/dev/null || true
      exit 0
    fi

    # --- dGPU layout ---
    if connected "DP-4" || connected "DP-2" || connected "HDMI-0"; then
      log "detected dGPU connectors -> applying dGPU layout"

      if connected "DP-2"; then
        ${XR} --output DP-2 --mode 1920x1080 --rate 60 --pos 0x0 --rotate left || true
      fi

      if connected "DP-4"; then
        ${XR} --output DP-4 --primary --mode 1920x1080 --rate 60 --pos 1080x420 --rotate normal || true
      fi

      # HDMI: hier kannst du entscheiden:
      # - wenn du beim Login HDMI AUS willst:
      # ${XR} --output HDMI-0 --off 2>/dev/null || true
      #
      # - wenn du beim Login HDMI an haben willst (z.B. gespiegelt in 1080p):
      if connected "HDMI-0"; then
        ${XR} --output HDMI-0 --mode 1920x1080 --rate 60 --pos 1080x420 --rotate normal || true
      fi

      exit 0
    fi

    log "no known connector set found; leaving defaults"
    exit 0
  '';
in
{
  options.${namespace}.desktop.awesome = with types; {
    enable = mkBoolOpt false "Enable Awesome WM session managed by SDDM (X11).";

    autoLogin = {
      enable = mkBoolOpt false "Enable SDDM autologin into Awesome.";
      user = mkStrOpt userName "Autologin user name.";
    };

    theme = mkStrOpt "" "Optional SDDM theme name (empty = default).";
  };

  config = mkIf cfg.enable (mkMerge [
    {
      services.xserver.enable = true;
      services.xserver.libinput.enable = true;

      services.xserver.windowManager.awesome.enable = true;

      services.xserver.displayManager.sddm.enable = true;

      # wichtig für X11
      services.xserver.displayManager.sddm.wayland.enable = false;

      services.displayManager.defaultSession = "none+awesome";

      services.displayManager.autoLogin = {
        enable = cfg.autoLogin.enable;
        user = cfg.autoLogin.user;
      };

      # Hook: greift für SDDM’s X11 session
      services.xserver.displayManager.sddm.settings = {
        X11 = {
          DisplayCommand = "${xsetup}";
        };
      };
    }

    (mkIf (cfg.theme != "") {
      services.xserver.displayManager.sddm.theme = cfg.theme;
    })
  ]);
}
