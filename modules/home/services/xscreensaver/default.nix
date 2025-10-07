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
  #     -> optional: nach DIM Sekunden Bildschirm abdunkeln (xset dpms force off)
  #     -> danach (AFTER Sekunden) systemctl suspend
  # - Bei "Monitor is On" / UNBLANK: Timer/State zurücksetzen
  xssSuspendDPMS = pkgs.writeShellScript "xss-suspend-dpms.sh" ''
    set -eu

    CHECK_INTERVAL=${toString cfg.suspend.checkInterval}
    HOLD=${toString cfg.suspend.holdOffSeconds}
    AFTER=${toString cfg.suspend.afterOffSeconds}
    DIM=${toString cfg.suspend.dimAfterGreeterSeconds}
    SAVER_GRACE=15  # Sekunden bis Greeter-Fallback wenn DPMS-Off nicht erkannt wird

    XSET=${pkgs.xorg.xset}/bin/xset
    DMTOOL=/run/current-system/sw/bin/dm-tool
    SYSTEMCTL=${pkgs.systemd}/bin/systemctl
    GREP=${pkgs.gnugrep}/bin/grep
    SLEEP=${pkgs.coreutils}/bin/sleep
    LOGGER=${pkgs.util-linux}/bin/logger
    XSCMD=${pkgs.xscreensaver}/bin/xscreensaver-command

    PENDING_SUSPEND=""
    PENDING_DIM=""
    OFF_SINCE=0
    GREETERED=0
    SAVER_ACTIVE=0
    SAVER_WAIT=0

    log() { "$LOGGER" -t xss-suspend -- "$*"; echo "$*"; }
    log "start: DISPLAY=$DISPLAY XAUTHORITY=$XAUTHORITY XDG_SEAT=$XDG_SEAT AFTER=$AFTER HOLD=$HOLD DIM=$DIM INT=$CHECK_INTERVAL"

    # Saver-Events nebenher beobachten
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

        # Dim-Timer nach Greeter
        if [ "$DIM" -gt 0 ]; then
          if [ -z "$PENDING_DIM" ] || ! kill -0 "$PENDING_DIM" 2>/dev/null; then
            log "arming dim in $DIM s"
            ( "$SLEEP" "$DIM" && "$XSET" dpms force off ) &
            PENDING_DIM=$!
          fi
        fi
      fi
    }

    arm_suspend_once() {
      if [ -z "$PENDING_SUSPEND" ] || ! kill -0 "$PENDING_SUSPEND" 2>/dev/null; then
        log "arming suspend in $AFTER s"
        ( "$SLEEP" "$AFTER" && log "suspending now" && "$SYSTEMCTL" suspend ) &
        PENDING_SUSPEND=$!
      fi
    }

    cancel_timers() {
      if [ -n "$PENDING_SUSPEND" ] && kill -0 "$PENDING_SUSPEND" 2>/dev/null; then
        kill "$PENDING_SUSPEND" 2>/dev/null || :
        PENDING_SUSPEND=""
        log "cancelled pending suspend"
      fi
      if [ -n "$PENDING_DIM" ] && kill -0 "$PENDING_DIM" 2>/dev/null; then
        kill "$PENDING_DIM" 2>/dev/null || :
        PENDING_DIM=""
        log "cancelled pending dim"
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

        # Aufwachen / Interaktion -> reset
        if [ "$GREETERED" -eq 1 ] && [ "$SAVER_ACTIVE" -eq 0 ]; then
          log "reset post-greeter"
          GREETERED=0
          cancel_timers()
        fi
      fi

      "$SLEEP" "$CHECK_INTERVAL"
    done
  '';
in {
  options.${namespace}.services.xscreensaver = with types; {
    enable = mkBoolOpt false "Enable XScreenSaver daemon.";

    # Reaktion auf Inaktivität: sofort Greeter, optional dimmen, später Suspend
    suspend = {
      enable = mkBoolOpt false "Switch to LightDM greeter on inactivity (DPMS-Off or Saver fallback), optionally dim, then suspend after a delay.";
      afterOffSeconds = mkOpt int 900 "Seconds to wait after greeter before suspending (0 = sofort).";
      dimAfterGreeterSeconds =
        mkOpt int 8
        "Seconds after greeter to force DPMS Off (0 = kein Dim-Schritt).";
      checkInterval = mkOpt int 5 "Polling interval for DPMS state (seconds).";
      holdOffSeconds = mkOpt int 10 "DPMS Off must persist this long before greeter/suspend are armed.";
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
        Description = "Greeter on inactivity (DPMS/Saver), dim after ${toString cfg.suspend.dimAfterGreeterSeconds}s, suspend after ${toString cfg.suspend.afterOffSeconds}s";
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
      Install = {
        WantedBy = ["graphical-session.target"];
      };
    };
  };
}
