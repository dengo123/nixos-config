-- ~/.config/awesome/shell/launchers/run/providers.lua
local awful = require("awful")

local P = {}

-- URL-Encode für Websuche
local function urlencode(s)
	if not s then
		return ""
	end
	s = s:gsub("\n", " "):gsub("([^%w%-%_%.%~ ])", function(c)
		return string.format("%%%02X", string.byte(c))
	end)
	return s:gsub(" ", "+")
end

-- Regex-escape für grep -E
local function re_escape(s)
	if not s then
		return ""
	end
	return (s:gsub("([%^%$%(%)%%%.%[%]%*%+%-%?])", "%%%1"))
end

-- ---------- Apps (Run) ----------
function P.run_search(query)
	local q = (query or ""):match("^%s*(.-)%s*$")
	if q == "" then
		return
	end

	local q_escaped = re_escape(q)

	-- WICHTIG: [=[ ... ]=] statt [[ ... ]] wegen "]]" in der Regex
	local sh = string.format(
		[=[
set -eu
Q=%q
paths="${XDG_DATA_HOME:-$HOME/.local/share}/applications /usr/share/applications"

list_files() {
  if command -v fd >/dev/null 2>&1; then
    fd -t f -e desktop . $paths
  elif command -v fdfind >/dev/null 2>&1; then
    fdfind -t f -e desktop . $paths
  else
    find $paths -type f -name '*.desktop' 2>/dev/null
  fi
}

# 1) schneller Dateiname-Containment
pick="$(list_files | awk -v q="$Q" 'BEGIN{IGNORECASE=1}
{
  f=$0; sub(/.*\//,"",f)
  if (index(f,q)) { print $0; exit }
}
')"

# 2) Fallback: Inhalt (Name[...] oder Exec enthält Q, case-insensitive)
if [ -z "${pick:-}" ] && command -v grep >/dev/null 2>&1; then
  # ^Name oder ^Name[xx]=  ODER  ^Exec=
  pick="$(list_files | xargs -r grep -IlE "(^Name(\[[^]]*\])?=.*%s.*|^Exec=.*%s.*)" 2>/dev/null | head -n1 || true)"
fi

[ -n "${pick:-}" ] || exit 0

base="${pick##*/}"
app="${base%%.desktop}"   # doppelt %% damit Lua-formatter nicht stolpert

# starten mit gtk-launch; Fallback dex/gio
if command -v gtk-launch >/dev/null 2>&1; then
  exec gtk-launch -- "$app" >/dev/null 2>&1 &
elif command -v dex >/dev/null 2>&1; then
  exec dex "$pick" >/dev/null 2>&1 &
else
  exec gio open "$pick" >/dev/null 2>&1 &
fi
]=],
		q,
		q_escaped,
		q_escaped
	)

	awful.spawn.with_shell(sh)
end

-- ---------- Files (Terminal im Zielordner) ----------
function P.local_search(query, HOME)
	HOME = HOME or (os.getenv("HOME") or "~")
	local q = (query or ""):match("^%s*(.-)%s*$")

	-- Ebenfalls [=[ ... ]=] wegen \~ etc.
	local sh = string.format(
		[=[
set -eu
HOME_DIR=%q
Q=%q

case "$Q" in "~"|"~/"*) Q="${Q/#\~/$HOME_DIR}";; esac
if [ "${Q#/}" != "$Q" ]; then CAND="$Q"; else CAND="$HOME_DIR/$Q"; fi

if [ -d "${CAND:-}" ]; then
  TARGET="$CAND"
elif [ -e "${CAND:-}" ] ; then
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

-- ---------- Web ----------
function P.web_search(query, ENGINE_FMT, BROWSER)
	local q = (query or ""):gsub("^%?+", ""):match("^%s*(.-)%s*$")
	if q == "" then
		return
	end
	local url = string.format(ENGINE_FMT or "https://duckduckgo.com/?q=%s", urlencode(q))
	awful.spawn({ BROWSER or "firefox", url }, false)
end

return P
