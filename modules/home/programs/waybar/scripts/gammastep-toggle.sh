#!/usr/bin/env bash

# Symbol: Sonne (☀) für inaktiv, Halbmond (🌙) für aktiv
ICON_ON="🌙"
ICON_OFF="☀"

# Gammastep executable
GAMMASTEP_BIN="$(command -v gammastep)"

# PID-Datei (für manuelles Starten)
PIDFILE="$XDG_RUNTIME_DIR/gammastep.pid"

# Toggle mode
if [[ "$1" == "toggle" ]]; then
    if pgrep -x gammastep >/dev/null; then
        pkill -x gammastep
        rm -f "$PIDFILE"
    else
        nohup "$GAMMASTEP_BIN" >/dev/null 2>&1 &
        echo $! > "$PIDFILE"
    fi
    sleep 0.3
fi

# Status check
if pgrep -x gammastep >/dev/null; then
    echo "{\"text\": \"$ICON_ON\"}"
else
    echo "{\"text\": \"$ICON_OFF\"}"
fi

