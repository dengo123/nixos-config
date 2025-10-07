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
  # - DPMS "Monitor is Off" stabil (>= HOLD):
  #     -> armt einmalig einen Suspend-Timer (AFTER Sekunden)
  # - Fallback: Saver aktiv ohne DPMS-Off (Grace):
  #     -> ebenfalls einmalig Suspend-Timer
  # - Abbruch, wenn Monitor wieder an ODER Saver nicht mehr aktiv:
  #     -> Timer wird verworfen
  #
  # Hinweis: KEIN Greeter/Lock vor Suspend. Der Greeter kommt NACH Resume
  # durch das systemweite Security/Resume-Hook (loginctl lock-sessions).
  xssSuspendDPMS = pkgs.writeShellScript "xss-suspend-dpms.sh" ''
    set -eu

    CHECK_INTERVAL=${toString cfg.suspend.checkInterval}
    HOLD=${toString cfg.suspend.holdOffSeconds}
    AFTER=${toString cfg.suspend.afterOffSeconds}
    SAVER_GRACE=15

    XSET=${pkgs.xorg.xset}/bin/xset
    SYSTEMCTL=${pkgs.systemd}/bin/systemctl
    GREP=${pkgs.gnugrep}/bin/grep
    SLEEP=${pkgs.coreutils}/bin/sleep
    LOGGER=${pkgs.util-linux}/bin/logger
    XSCMD=${pkgs.xscreensaver}/bin/xscreensaver-command

    PENDING=""
    OFF_SINCE=0
    SAVER_ACTIVE=0
    SAVER_WAIT=0

    log() { "$LOGGER" -t xss-suspend -- "$*"; echo "$*"; }

    log "start: DISPLAY=$DISPLAY XAUTHORITY=$XAUTHORITY AFTER=$AFTER HOLD=$HOLD INT=$CHECK_INTERVAL"

    # Saver-Events beobachten (parallel)
    ( "$XSCMD" -watch | while read -r line; do
        case "$line" in
          BLANK*|LOCK*|RUN*) SAVER_ACTIVE=1; SAVER_WAIT=0 ;;
          UNBLANK*|UNLOCK*)  SAVER_ACTIVE=0; SAVER_WAIT=0 ;;
        esac
      done ) &

    arm_suspend_once() {
      if [ -z "$PENDING" ] || ! kill -0 "$PENDING" 2>/dev/null; then
        if [ "$AFTER" -le 0 ]; then
          log "suspending now"
          ( "$SYSTEMCTL" suspend ) &
        else
          log "arming suspend in $AFTER s"
          ( "$SLEEP" "$AFTER" && log "suspending now" && "$SYSTEMCTL" suspend ) &
        fi
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
          arm_suspend_once
        fi
      else
        OFF_SINCE=0

        # Fallback: Saver aktiv, aber kein DPMS-Off -> nach Grace dennoch Timer
        if [ "$SAVER_ACTIVE" -eq 1 ]; then
          SAVER_WAIT=$((SAVER_WAIT + CHECK_INTERVAL))
          if [ "$SAVER_WAIT" -ge "$SAVER_GRACE" ]; then
            log "screensaver active but no DPMS-Off -> arming suspend (fallback)"
            arm_suspend_once
          fi
        else
          # Weder DPMS-Off noch Saver aktiv -> evtl. scharfen Timer verwerfen
          SAVER_WAIT=0
          cancel_suspend
        fi
      fi

      "$SLEEP" "$CHECK_INTERVAL"
    done
  '';
in {
  options.${namespace}.services.xscreensaver = with types; {
    enable = mkBoolOpt false "Enable XScreenSaver daemon.";

    # Alte Struktur wiederhergestellt:
    suspend = {
      enable = mkBoolOpt false "Suspend the system on DPMS-Off (with optional timer).";
      afterOffSeconds =
        mkOpt int 900
        "Seconds to wait after DPMS-Off before suspending (0 = immediately).";
      checkInterval = mkOpt int 5 "Polling interval for DPMS state (seconds).";
      holdOffSeconds =
        mkOpt int 10
        "Require DPMS Off to persist this long before the suspend timer is armed.";
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
    ];

    systemd.user.services."xss-suspend" = mkIf cfg.suspend.enable {
      Unit = {
        Description = "Suspend on DPMS-Off/Saver inactivity (no greeter pre-suspend)";
        After = ["graphical-session.target"];
        PartOf = ["graphical-session.target"];
      };
      Service = {
        Environment = [
          "DISPLAY=:0"
          "XAUTHORITY=%h/.Xauthority"
        ];
        ExecStart = xssSuspendDPMS;
        Restart = "always";
        RestartSec = 1;
      };
      Install.WantedBy = ["graphical-session.target"];
    };
  };
}
