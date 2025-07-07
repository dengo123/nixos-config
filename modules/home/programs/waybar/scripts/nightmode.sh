#!/usr/bin/env bash

ICON_ON=""
ICON_OFF="󰖨"
TEMP=3000
PIDFILE="$XDG_RUNTIME_DIR/hyprsunset.pid"

# Toggle
if [[ "$1" == "toggle" ]]; then
  if pgrep -x hyprsunset >/dev/null; then
    pkill -x hyprsunset
    rm -f "$PIDFILE"
  else
    nohup hyprsunset -t $TEMP >/dev/null 2>&1 &
    echo $! > "$PIDFILE"
  fi
  sleep 0.2
fi

# Status
if pgrep -x hyprsunset >/dev/null; then
  echo "{\"text\": \"$ICON_ON\", \"tooltip\": \"Nightmode aktiv\"}"
else
  echo "{\"text\": \"$ICON_OFF\", \"tooltip\": \"Nightmode aus\"}"
fi

