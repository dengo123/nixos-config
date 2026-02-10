# modules/nixos/desktop/awesome/default.nix
{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.desktop.awesome;
  userName = config.${namespace}.config.user.name or "dengo123";

  XR = "${pkgs.xorg.xrandr}/bin/xrandr";

  # "dumm" = nur sicherstellen, dass X nicht mit kaputtem Layout startet.
  # Autorandr soll später alles übernehmen (hotplug HDMI<->TV etc.)
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

    # persistent log (helpful for first boot / black screen)
    exec >>/var/log/sddm-xsetup.log 2>&1
    date
    echo "sddm-xsetup: start"

    # Wait until X enumerated outputs (avoid race on cold boot)
    for i in $(seq 1 50); do
      if ${XR} --query >/dev/null 2>&1 && ${XR} --query | grep -q " connected"; then
        break
      fi
      sleep 0.2
    done

    # Big framebuffer to avoid "not large enough" when HDMI is 4K
    ${XR} --fb 8192x4320 || true

    # === DUMB MODE ===
    # 1) Enable preferred outputs if present (your current wiring)
    #    DP-0 = portrait monitor (usually), HDMI-0 = landscape monitor/TV hotplug
    # 2) Do NOT force positions/rotations here.
    # 3) Ensure exactly one primary (prefer DP-0 if connected, else HDMI-0, else first connected).
    connected() { ${XR} --query | grep -q "^$1 connected"; }

    # Turn on the outputs we care about if they exist; ignore errors.
    if connected "DP-0"; then
      ${XR} --output DP-0 --auto || true
    fi
    if connected "HDMI-0"; then
      ${XR} --output HDMI-0 --auto || true
    fi

    # Pick a safe primary
    if connected "HDMI-A-0"; then
      ${XR} --output HDMI-A-0 --primary || true
    elif connected "HDMI-0"; then
      ${XR} --output HDMI-0 --primary || true
    else
      PRIMARY="$(${XR} --query | awk '/ connected/{print $1; exit}')"
      if [ -n "$PRIMARY" ]; then
        ${XR} --output "$PRIMARY" --primary --auto || true
      fi
    fi

    echo "sddm-xsetup: end"
    exit 0
  '';
in {
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

      services.displayManager.defaultSession = "none+awesome";

      services.displayManager.autoLogin = {
        enable = cfg.autoLogin.enable;
        user = cfg.autoLogin.user;
      };

      services.displayManager.sddm.enable = true;
      services.displayManager.sddm.wayland.enable = false;

      # "dumb but safe": just ensure one output is on + primary.
      services.displayManager.sddm.settings = {
        X11 = {
          DisplayCommand = "${xsetup}";
        };
      };
    }

    (mkIf (cfg.theme != "") {
      services.displayManager.sddm = {
        theme = cfg.theme;
      };
    })
  ]);
}
