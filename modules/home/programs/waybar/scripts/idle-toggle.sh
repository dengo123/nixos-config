#!/usr/bin/env bash

PIDFILE="/tmp/waybar-idle-inhibit.pid"

is_active() {
  [[ -f "$PIDFILE" ]] && kill -0 "$(cat "$PIDFILE")" 2>/dev/null
}

toggle() {
  if is_active; then
    kill "$(cat "$PIDFILE")"
    rm "$PIDFILE"
  else
    systemd-inhibit --what=idle:sleep --why="Waybar Idle Inhibitor" --mode=block sleep infinity &
    echo $! > "$PIDFILE"
  fi
}

status_json() {
  if is_active; then
    echo '{"text": "󰈈", "tooltip": "Idle Inhibited"}'
  else
    echo '{"text": "󰈉", "tooltip": "Idle Active"}'
  fi
}

case "$1" in
  toggle)
    toggle
    ;;
  *)
    status_json
    ;;
esac

