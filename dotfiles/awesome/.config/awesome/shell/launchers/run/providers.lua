-- ~/.config/awesome/shell/launchers/run/providers.lua
local awful = require("awful")

local P = {}

-- -------- helpers --------
local function urlencode(s)
	if not s then
		return ""
	end
	s = s:gsub("\n", " "):gsub("([^%w%-%_%.%~ ])", function(c)
		return string.format("%%%02X", string.byte(c))
	end)
	return s:gsub(" ", "+")
end

local function sh_quote(s) -- POSIX-Quoting
	s = tostring(s or "")
	return "'" .. s:gsub("'", "'\"'\"'") .. "'"
end

-- -------- Apps (Run) --------
function P.run_search(query)
	local q = (query or ""):match("^%s*(.-)%s*$")
	if q == "" then
		return
	end

	local tmpl = [=[#!/bin/sh
set -eu
Q=__Q__
paths="${XDG_DATA_HOME:-$HOME/.local/share}/applications /usr/share/applications /usr/local/share/applications $HOME/.local/share/flatpak/exports/share/applications /var/lib/flatpak/exports/share/applications /var/lib/snapd/desktop/applications"

list_files() {
  if command -v fd >/dev/null 2>&1; then
    fd -t f -e desktop . $paths
  elif command -v fdfind >/dev/null 2>&1; then
    fdfind -t f -e desktop . $paths
  else
    find $paths -type f -name '*.desktop' 2>/dev/null
  fi
}

# 1) Dateiname enthält Q (case-insensitive)
pick="$(list_files | awk -v q="$Q" 'BEGIN{IGNORECASE=1}{
  f=$0; sub(/.*\//,"",f);
  if (index(f,q)) { print $0; exit }
}')"

# 2) Fallback: Name/Exec enthält Q (fixed-string → robust gg. Sonderzeichen)
if [ -z "${pick:-}" ] && command -v grep >/dev/null 2>&1; then
  pick="$(list_files | xargs -r grep -IlF 'Name=' 2>/dev/null | xargs -r grep -IlF "$Q" 2>/dev/null | head -n1 || true)"
  if [ -z "${pick:-}" ]; then
    pick="$(list_files | xargs -r grep -IlF 'Exec=' 2>/dev/null | xargs -r grep -IlF "$Q" 2>/dev/null | head -n1 || true)"
  fi
fi

# 3) Kein .desktop? → direkt versuchen (wie Awesome-Default)
if [ -z "${pick:-}" ]; then
  if command -v "$Q" >/dev/null 2>&1; then
    nohup "$Q" >/dev/null 2>&1 &
    exit 0
  fi
  nohup sh -c "$Q" >/dev/null 2>&1 || true &
  exit 0
fi

base="${pick##*/}"
app="${base%%.desktop}"

launched=""

# 4) gtk-launch (ohne --)
if command -v gtk-launch >/dev/null 2>&1; then
  gtk-launch "$app" >/dev/null 2>&1 & launched=1
fi

# 5) dex Fallback
if [ -z "${launched:-}" ] && command -v dex >/dev/null 2>&1; then
  dex "$pick" >/dev/null 2>&1 & launched=1
fi

# 6) Exec= parsen und ausführen
if [ -z "${launched:-}" ]; then
  exec_line="$(grep -m1 -E '^Exec=' "$pick" | sed 's/^Exec=//')"
  exec_cmd="$(printf '%s' "$exec_line" \
    | sed -E 's/%[fFuUdDnNickvm]//g' \
    | sed -E 's/%[FURL]//g' \
    | sed -E 's/%./ /g')"
  if [ -n "$exec_cmd" ]; then
    nohup sh -c "$exec_cmd" >/dev/null 2>&1 &
    launched=1
  fi
fi

# 7) Letzte Stufe: Befehl selbst
if [ -z "${launched:-}" ]; then
  if command -v "$Q" >/dev/null 2>&1; then
    nohup "$Q" >/dev/null 2>&1 &
  else
    nohup sh -c "$Q" >/dev/null 2>&1 || true &
  fi
fi
]=]

	awful.spawn.with_shell(tmpl:gsub("__Q__", sh_quote(q)))
end

-- -------- Files (zoxide-first) --------
function P.local_search(query, HOME)
	HOME = HOME or (os.getenv("HOME") or "~")
	local q = (query or ""):match("^%s*(.-)%s*$")

	local sh = string.format(
		[=[
set -eu
HOME_DIR=%q
Q=%q

# 0) zoxide bevorzugen
if command -v zoxide >/dev/null 2>&1; then
  Z="$(zoxide query -- "$Q" 2>/dev/null || true)"
  if [ -n "${Z:-}" ] && [ -d "$Z" ]; then TARGET="$Z"; fi
fi

if [ -z "${TARGET:-}" ]; then
  case "$Q" in "~"|"~/"*) Q="${Q/#\~/$HOME_DIR}";; esac
  if [ "${Q#/}" != "$Q" ]; then CAND="$Q"; else CAND="$HOME_DIR/$Q"; fi

  if [ -d "${CAND:-}" ]; then
    TARGET="$CAND"
  elif [ -e "${CAND:-}" ]; then
    TARGET="$(dirname -- "$CAND")"
  else
    BIN="$(command -v fd || command -v fdfind || true)"
    if [ -n "${BIN:-}" ]; then
      FIRST="$("$BIN" -H -c never -- "$Q" "$HOME_DIR" 2>/dev/null | head -n1 || true)"
    else
      FIRST="$(find "$HOME_DIR" -iname "*$Q*" -print -quit 2>/dev/null || true)"
    fi
    if [ -n "${FIRST:-}" ]; then
      [ -d "$FIRST" ] && TARGET="$FIRST" || TARGET="$(dirname -- "$FIRST")"
    else
      TARGET="$HOME_DIR"
    fi
  fi
fi

term="${TERMINAL:-}"
if [ -z "$term" ]; then
  for t in kitty alacritty gnome-terminal konsole xfce4-terminal xterm; do
    if command -v "$t" >/dev/null 2>&1; then term="$t"; break; fi
  done
fi
term="${term:-xterm}"

case "$term" in
  kitty)            exec kitty --directory "$TARGET" >/dev/null 2>&1 & ;;
  alacritty)        exec alacritty --working-directory "$TARGET" >/dev/null 2>&1 & ;;
  gnome-terminal)   exec gnome-terminal --working-directory="$TARGET" >/dev/null 2>&1 & ;;
  konsole)          exec konsole --workdir "$TARGET" >/dev/null 2>&1 & ;;
  xfce4-terminal)   exec xfce4-terminal --working-directory "$TARGET" >/dev/null 2>&1 & ;;
  xterm)            (cd "$TARGET" && exec xterm) >/dev/null 2>&1 & ;;
  *)                (cd "$TARGET" && exec "$term") >/dev/null 2>&1 & ;;
esac
]=],
		HOME,
		q
	)

	awful.spawn.with_shell(sh)
end

-- -------- Web --------
function P.web_search(query, ENGINE_FMT, BROWSER)
	local q = (query or ""):gsub("^%?+", ""):match("^%s*(.-)%s*$")
	if q == "" then
		return
	end
	local url = string.format(ENGINE_FMT or "https://duckduckgo.com/?q=%s", urlencode(q))
	awful.spawn({ BROWSER or "firefox", url }, false)
end

return P
