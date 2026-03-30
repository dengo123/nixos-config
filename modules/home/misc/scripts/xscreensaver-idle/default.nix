# modules/home/misc/scripts/xscreensaver-idle/default.nix
{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.misc.scripts.xscreensaver-idle;

  script = pkgs.writeShellScriptBin "xscreensaver-idle" ''
    #!/usr/bin/env bash
    set -euo pipefail

    LOCK_ENABLED="${
      if cfg.lock.enable
      then "1"
      else "0"
    }"
    LOCK_DELAY="${toString cfg.lock.delay}"
    LOCK_CMD=${escapeShellArg cfg.lock.command}

    SUSPEND_ENABLED="${
      if cfg.suspend.enable
      then "1"
      else "0"
    }"
    SUSPEND_DELAY="${toString cfg.suspend.delay}"
    SUSPEND_CMD=${escapeShellArg cfg.suspend.command}

    LOGGER="${pkgs.util-linux}/bin/logger"
    XSCMD="${pkgs.xscreensaver}/bin/xscreensaver-command"
    SLEEP="${pkgs.coreutils}/bin/sleep"
    BASH_BIN="${pkgs.bash}/bin/bash"

    pending_lock_pid=""
    pending_suspend_pid=""
    blank_active="0"
    lock_fired="0"

    log() {
      "$LOGGER" -t xscreensaver-idle -- "$*"
    }

    start_lock_timer() {
      [ "$LOCK_ENABLED" = "1" ] || return 0
      [ -n "$LOCK_CMD" ] || return 0
      [ "$lock_fired" = "0" ] || return 0

      if [ -n "$pending_lock_pid" ] && kill -0 "$pending_lock_pid" 2>/dev/null; then
        return 0
      fi

      log "starting lock timer ($LOCK_DELAY s)"
      (
        "$SLEEP" "$LOCK_DELAY"
        log "running lock command"
        "$BASH_BIN" -lc "$LOCK_CMD"
      ) &
      pending_lock_pid=$!
    }

    cancel_lock_timer() {
      if [ -n "$pending_lock_pid" ] && kill -0 "$pending_lock_pid" 2>/dev/null; then
        kill "$pending_lock_pid" 2>/dev/null || true
        wait "$pending_lock_pid" 2>/dev/null || true
        log "cancelled lock timer"
      fi
      pending_lock_pid=""
    }

    reap_lock_timer_if_finished() {
      if [ -n "$pending_lock_pid" ] && ! kill -0 "$pending_lock_pid" 2>/dev/null; then
        wait "$pending_lock_pid" 2>/dev/null || true
        pending_lock_pid=""
        lock_fired="1"
        log "lock timer finished"
      fi
    }

    start_suspend_timer() {
      [ "$SUSPEND_ENABLED" = "1" ] || return 0

      if [ -n "$pending_suspend_pid" ] && kill -0 "$pending_suspend_pid" 2>/dev/null; then
        return 0
      fi

      log "starting suspend timer ($SUSPEND_DELAY s)"
      (
        "$SLEEP" "$SUSPEND_DELAY"
        log "running suspend command"
        "$BASH_BIN" -lc "$SUSPEND_CMD"
      ) &
      pending_suspend_pid=$!
    }

    cancel_suspend_timer() {
      if [ -n "$pending_suspend_pid" ] && kill -0 "$pending_suspend_pid" 2>/dev/null; then
        kill "$pending_suspend_pid" 2>/dev/null || true
        wait "$pending_suspend_pid" 2>/dev/null || true
        log "cancelled suspend timer"
      fi
      pending_suspend_pid=""
    }

    cleanup() {
      cancel_lock_timer
      cancel_suspend_timer
    }

    trap cleanup EXIT INT TERM

    log "starting xscreensaver idle hook"

    while read -r line; do
      reap_lock_timer_if_finished

      case "$line" in
        BLANK*)
          log "event: BLANK"

          if [ "$blank_active" = "1" ]; then
            log "ignoring duplicate BLANK"
            continue
          fi

          blank_active="1"
          lock_fired="0"

          start_lock_timer
          start_suspend_timer
          ;;

        UNBLANK*)
          log "event: UNBLANK"

          blank_active="0"

          cancel_lock_timer
          cancel_suspend_timer
          ;;

        LOCK*|RUN*)
          log "event: $line"
          ;;

        *)
          :
          ;;
      esac
    done < <("$XSCMD" -watch)
  '';
in {
  options.${namespace}.misc.scripts.xscreensaver-idle = with types; {
    enable = mkBoolOpt false "Enable the xscreensaver-idle helper script and user service.";

    lock = {
      enable = mkBoolOpt true "Start a lock timer on the first xscreensaver BLANK event.";

      delay = mkOpt int 300 "Seconds to wait after the first BLANK before running the lock command.";

      command = mkOpt str "XDG_SEAT_PATH=/org/freedesktop/DisplayManager/Seat0 ${pkgs.lightdm}/bin/dm-tool lock" "Command to run after the lock delay.";
    };

    suspend = {
      enable = mkBoolOpt false "Start a suspend timer on the first xscreensaver BLANK and cancel it on UNBLANK.";

      delay = mkOpt int 900 "Seconds to wait after the first BLANK before running the suspend command.";

      command = mkOpt str "${pkgs.systemd}/bin/systemctl suspend" "Command to run after the suspend delay.";
    };

    package = mkOpt package script "Generated xscreensaver-idle script package.";
  };

  config = mkIf cfg.enable {
    home.packages = [
      cfg.package
      pkgs.xscreensaver
    ];

    systemd.user.services.xscreensaver-idle = {
      Unit = {
        Description = "XScreenSaver idle hook";
        After = ["graphical-session.target"];
        PartOf = ["graphical-session.target"];
      };

      Service = {
        ExecStart = "${cfg.package}/bin/xscreensaver-idle";
        Restart = "always";
        RestartSec = 2;
        Environment = [
          "DISPLAY=:0"
          "XAUTHORITY=%h/.Xauthority"
        ];
      };

      Install = {
        WantedBy = ["graphical-session.target"];
      };
    };
  };
}
