#!/usr/bin/env bash

STATEFILE="/tmp/waybar-idle-inhibitor"

get_state() {
  if [[ -f "$STATEFILE" && "$(cat "$STATEFILE")" == "1" ]]; then
    echo "1"
  else
    echo "0"
  fi
}

toggle() {
  if [[ "$(get_state)" == "1" ]]; then
    echo 0 > "$STATEFILE"
    swayidle -w &
    pkill swayidle
  else
    echo 1 > "$STATEFILE"
    pkill swayidle
  fi
}

output_json() {
  if [[ "$(get_state)" == "1" ]]; then
    echo '{"text": "󰈈", "tooltip": "Presentation Mode"}'
  else
    echo '{"text": "󰈉", "tooltip": "Idle Mode"}'
  fi
}

case "$1" in
  toggle)
    toggle
    ;;
  *)
    output_json
    ;;
esac

