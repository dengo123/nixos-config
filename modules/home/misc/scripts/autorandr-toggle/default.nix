# modules/home/misc/scripts/autorandr-toggle/default.nix
{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.misc.scripts.autorandr-toggle;

  autorandrToggle = pkgs.writeShellScriptBin "autorandr-toggle" ''
    #!/usr/bin/env bash
    set -euo pipefail

    WORK="${cfg.workProfile}" # "dual"
    TV="${cfg.tvProfile}"     # "tv"

    LOG="/tmp/autorandr-apply.$USER.log"
    LOCK="/tmp/autorandr-toggle.$USER.lock"

    have() { command -v "$1" >/dev/null 2>&1; }

    notify() {
      if have notify-send; then
        notify-send "autorandr-toggle" "$1" >/dev/null 2>&1 || true
      fi
    }

    awesome_sig() {
      if have awesome-client; then
        awesome-client "awesome.emit_signal(\"$1\")" >/dev/null 2>&1 || true
      fi
    }

    # ---- X env: Keybinds laufen manchmal ohne DISPLAY/XAUTHORITY ----
    export HOME="''${HOME:-${config.home.homeDirectory}}"
    export XDG_CONFIG_HOME="''${XDG_CONFIG_HOME:-$HOME/.config}"
    export DISPLAY="''${DISPLAY:-:0}"
    export XAUTHORITY="''${XAUTHORITY:-$HOME/.Xauthority}"

    ts() { date --iso-8601=seconds 2>/dev/null || date; }

    log_block() {
      mkdir -p "$(dirname "$LOG")" 2>/dev/null || true
      {
        echo "=== ''$(ts) APPLY $1 ==="
        echo "DISPLAY=$DISPLAY"
        echo "XAUTHORITY=$XAUTHORITY"
        echo "--- xrandr query ---"
        xrandr --query 2>&1 || true
        echo "--- autorandr --current ---"
        autorandr --current 2>&1 || true
        echo "--- autorandr --detected ---"
        autorandr --detected 2>&1 || true
        echo "--- autorandr --load ---"
      } >>"$LOG"
    }

    # Single instance (Spam durch Keyrepeat verhindern)
    if ! ( set -o noclobber; echo "$$" >"$LOCK" ) 2>/dev/null; then
      exit 0
    fi
    trap 'rm -f "$LOCK"' EXIT

    # Entscheide anhand des aktuellen Primary-Outputs
    primary="$(xrandr --query | awk '/ connected primary/{print $1; exit}')"
    if [ "$primary" = "HDMI-0" ]; then
      next="$WORK"
    else
      next="$TV"
    fi

    notify "switch -> $next"
    awesome_sig "autorandr::pre"
    log_block "$next"

    # apply mit retry
    ok=0
    for i in 1 2 3; do
      if autorandr --load "$next" --force >>"$LOG" 2>&1; then
        ok=1
        break
      fi
      sleep 0.25
    done

    if [ "$ok" -ne 1 ]; then
      notify "FAILED: $next (siehe $LOG)"
      awesome_sig "autorandr::failed"
      exit 1
    fi

    # verifizieren (kurz warten, dann primary nochmal lesen)
    sleep 0.20
    primary2="$(xrandr --query | awk '/ connected primary/{print $1; exit}')"

    # Wenn wir tv wollten, muss HDMI-0 primary sein; wenn dual, muss es NICHT HDMI-0 sein (oder DP-4)
    if [ "$next" = "$TV" ] && [ "$primary2" != "HDMI-0" ]; then
      echo "WARN: expected HDMI-0 primary but got $primary2" >>"$LOG"
    fi
    if [ "$next" = "$WORK" ] && [ "$primary2" = "HDMI-0" ]; then
      echo "WARN: expected non-HDMI-0 primary but got HDMI-0" >>"$LOG"
    fi

    awesome_sig "autorandr::applied"
  '';
in {
  options.${namespace}.misc.scripts.autorandr-toggle = with types; {
    enable = mkBoolOpt false "Enable misc.scripts.autorandr-toggle";
    workProfile = mkStrOpt "dual" "Autorandr profile name for WORK/dual layout.";
    tvProfile = mkStrOpt "tv" "Autorandr profile name for TV layout.";
  };

  config = mkIf cfg.enable {
    home.packages = [
      pkgs.autorandr
      pkgs.xrandr
      autorandrToggle
    ];
  };
}
