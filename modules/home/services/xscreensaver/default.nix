# modules/home/services/xscreensaver/default.nix
{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.services.xscreensaver;

  # DPMS/Saver-Watcher:
  # - DPMS "Monitor is Off" stabil (>= HOLD) ODER Saver aktiv ohne DPMS-Off (Grace):
  #     -> genau einmal zum LightDM-Greeter (dm-tool switch-to-greeter, Fallback lock)
  #     -> danach (AFTER Sekunden) systemctl suspend
  # - Abbruch NUR wenn Session entsperrt (LockedHint=no), nicht bloß beim Verlassen des Savers.
  xssSuspendDPMS = pkgs.writeShellScript "xss-suspend-dpms.sh" ''
    set -eu

    CHECK_INTERVAL=${toString cfg.suspend.checkInterval}
    HOLD=${toString cfg.suspend.holdOffSeconds}
    AFTER=${toString cfg.suspend.afterOffSeconds}
    SAVER_GRACE=15

    XSET=${pkgs.xorg.xset}/bin/xset
    DMTOOL=/run/current-system/sw/bin/dm-tool
    SYSTEMCTL=${pkgs.systemd}/bin/systemctl
    GREP=${pkgs.gnugrep}/bin/grep
    SLEEP=${pkgs.coreutils}/bin/sleep
    LOGGER=${pkgs.util-linux}/bin/logger
    XSCMD=${pkgs.xscreensaver}/bin/xscreensaver-command
    LOGINCTL=${pkgs.systemd}/bin/loginctl

    PENDING=""
    OFF_SINCE=0
    GREETERED=0
    SAVER_ACTIVE=0
    SAVER_WAIT=0

    # Session-ID ermitteln (erst Env, sonst loginctl)
    SESS_ID="$XDG_SESSION_ID"
    if [ -z "$SESS_ID" ]; then
      SESS_ID="$("$LOGINCTL" list-sessions --no-legend | awk -v u="$USER" '$0 ~ u {print $1; exit}')"
    fi

    log() { "$LOGGER" -t xss-suspend -- "$*"; echo "$*"; }

    is_locked() {
      # 0 = gesperrt, 1 = entsperrt/unknown
      if [ -z "$SESS_ID" ]; then
        return 1
      fi
      hint="$("$LOGINCTL" show-session "$SESS_ID" -p LockedHint --value 2>/dev/null || echo no)"
      [ "$hint" = "yes" ]
    }

    SID_PRINT="$SESS_ID"; [ -z "$SID_PRINT" ] && SID_PRINT="?"
    log "start: DISPLAY=$DISPLAY XAUTHORITY=$XAUTHORITY XDG_SEAT=$XDG_SEAT SESS_ID=$SID_PRINT AFTER=$AFTER HOLD=$HOLD INT=$CHECK_INTERVAL"

    # Saver-Events beobachten (parallel)
    ( "$XSCMD" -watch | while read -r line; do
        case "$line" in
          BLANK*|LOCK*|RUN*) SAVER_ACTIVE=1; SAVER_WAIT=0 ;;
          UNBLANK*|UNLOCK*)  SAVER_ACTIVE=0; SAVER_WAIT=0 ;;
        esac
      done ) &

    switch_to_greeter_once() {
      if [ "$GREETERED" -eq 0 ]; then
        log "-> switching to greeter…"
        if "$DMTOOL" --seat seat0 switch-to-greeter 2>/dev/null \
           || XDG_SEAT_PATH=/org/freedesktop/DisplayManager/Seat0 "$DMTOOL" switch-to-greeter 2>/dev/null \
           || "$DMTOOL" --seat seat0 lock 2>/dev/null; then
          log "greeter/lock: OK"
        else
          log "dm-tool: FAILED"
        fi
        GREETERED=1
      fi
    }

    arm_suspend_once() {
      if [ -z "$PENDING" ] || ! kill -0 "$PENDING" 2>/dev/null; then
        log "arming suspend in $AFTER s"
        ( "$SLEEP" "$AFTER" && log "suspending now" && "$SYSTEMCTL" suspend ) &
        PENDING=$!
      fi
    }

    cancel_suspend() {
      if [ -n "$PENDING" ] && kill -0 "$PENDING" 2>/dev/null; then
        kill "$PENDING" 2>/dev/null || :
        PENDING=""
        log "cancelled pending suspend"
      fi
    }

    while true; do
      if "$XSET" q | "$GREP" -q "Monitor is Off"; then
        OFF_SINCE=$((OFF_SINCE + CHECK_INTERVAL))
        SAVER_WAIT=0
        if [ "$OFF_SINCE" -ge "$HOLD" ]; then
          switch_to_greeter_once
          arm_suspend_once
        fi
      else
        OFF_SINCE=0
        # Fallback: Saver aktiv, aber kein DPMS-Off -> nach Grace dennoch greeter+timer
        if [ "$SAVER_ACTIVE" -eq 1 ] && [ "$GREETERED" -eq 0 ]; then
          SAVER_WAIT=$((SAVER_WAIT + CHECK_INTERVAL))
          if [ "$SAVER_WAIT" -ge "$SAVER_GRACE" ]; then
            log "Saver active but no DPMS-Off -> fallback greeter"
            switch_to_greeter_once
            arm_suspend_once
          fi
        else
          SAVER_WAIT=0
        fi

        # Nach Greeter nur dann resetten, wenn die Session entsperrt ist
        if [ "$GREETERED" -eq 1 ]; then
          if ! is_locked; then
            log "session unlocked -> reset"
            GREETERED=0
            cancel_suspend
          fi
        fi
      fi

      "$SLEEP" "$CHECK_INTERVAL"
    done
  '';
in {
  options.${namespace}.services.xscreensaver = with types; {
    enable = mkBoolOpt false "Enable XScreenSaver daemon.";

    suspend = {
      enable = mkBoolOpt false "Switch to LightDM greeter on inactivity (DPMS-Off or Saver fallback), then suspend after a delay.";
      afterOffSeconds =
        mkOpt int 900
        "Seconds to wait after greeter before suspending (0 = immediately).";
      checkInterval = mkOpt int 5 "Polling interval for DPMS state (seconds).";
      holdOffSeconds =
        mkOpt int 10
        "Require DPMS Off to persist this long before greeter/suspend are armed.";
    };
  };

  config = mkIf cfg.enable {
    services.xscreensaver = {
      enable = true;
      package = pkgs.xscreensaver;
    };

    home.packages = [
      pkgs.xscreensaver
      pkgs.xorg.xset
      pkgs.lightdm
    ];

    systemd.user.services."xss-suspend" = mkIf cfg.suspend.enable {
      Unit = {
        Description = "LightDM greeter on inactivity, then suspend after ${toString cfg.suspend.afterOffSeconds}s";
        After = ["graphical-session.target"];
        PartOf = ["graphical-session.target"];
      };
      Service = {
        Environment = [
          "DISPLAY=:0"
          "XAUTHORITY=%h/.Xauthority"
          "XDG_SEAT=seat0"
          "XDG_SEAT_PATH=/org/freedesktop/DisplayManager/Seat0"
        ];
        ExecStart = xssSuspendDPMS;
        Restart = "always";
        RestartSec = 1;
      };
      Install.WantedBy = ["graphical-session.target"];
    };
  };
}
