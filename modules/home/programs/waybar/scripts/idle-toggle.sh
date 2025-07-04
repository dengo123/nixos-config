#!/usr/bin/env bash

# Toggle mode
if [ "$1" = "toggle" ]; then
  if pgrep -x hypridle >/dev/null; then
    pkill hypridle
  else
    nohup hypridle >/dev/null 2>&1 &
  fi

  # Trigger Waybar to refresh this module
  pkill -RTMIN+10 waybar
  exit 0
fi

# Read status
if pgrep -x hypridle >/dev/null; then
  echo '{"text": "󰈈", "tooltip": "Idle Inhibitor: Active"}'
else
  echo '{"text": "󰈉", "tooltip": "Idle Inhibitor: Inactive"}'
fi

