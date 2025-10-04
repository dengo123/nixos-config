-- ~/.config/awesome/shell/launchers/run/providers.lua
local awful = require("awful")

local P = {}

-- --- Apps: „run“ (DEFAULT) ---------------------------------------------------
-- Sucht .desktop Einträge (fd/fdfind optional) und startet mit gtk-launch.
-- Sehr simpel/fuzzy: Match auf Name/Exec/Dateiname.
local function urlencode(s)
	if not s then
		return ""
	end
	s = s:gsub("\n", " "):gsub("([^%w%-%_%.%~ ])", function(c)
		return string.format("%%%02X", string.byte(c))
	end)
	return s:gsub(" ", "+")
end

function P.run_search(query)
	local q = (query or ""):match("^%s*(.-)%s*$")
	if q == "" then
		return
	end

	local sh = string.format(
		[[
set -eu
Q=%q
paths="${XDG_DATA_HOME:-$HOME/.local/share}/applications /usr/share/applications"
list_files() { if command -v fd >/dev/null; then fd -t f -e desktop . $paths; elif command -v fdfind >/dev/null; then fdfind -t f -e desktop . $paths; else find $paths -type f -name '*.desktop'; fi; }
pick="$(list_files | awk -v q="$Q" 'BEGIN{IGNORECASE=1}
  {f=$0}
  sub(/.*\//,"",f)
  if (index(f,q)) { print $0; next }
  }
  END{}' | head -n1)"

# Fallback: grep Inhalt
if [ -z "$pick" ] && command -v grep >/dev/null; then
  pick="$(list_files | xargs -r grep -IlE "(^Name=.*%s.*|^Exec=.*%s.*)" | head -n1)"
fi

[ -n "$pick" ] || exit 0
base="${pick##*/}"; app="${base%.desktop}"
exec gtk-launch -- "$app" >/dev/null 2>&1 &
]],
		q,
		q
	)

	awful.spawn.with_shell(sh)
end

-- --- Files (wie gehabt) ------------------------------------------------------
function P.local_search(query, HOME)
	HOME = HOME or (os.getenv("HOME") or "~")
	local q = (query or ""):match("^%s*(.-)%s*$")
	if q == "" then
		awful.spawn({ "xdg-open", HOME }, false)
		return
	end

	local sh = string.format(
		[[
set -eu
HOME_DIR=%q
Q=%q
case "$Q" in "~"|"~/"*) Q="${Q/#\~/$HOME_DIR}";; esac
if [ "${Q#/}" != "$Q" ]; then CAND="$Q"; else CAND="$HOME_DIR/$Q"; fi
if [ -d "$CAND" ]; then printf "%%s\n" "$CAND"; exit 0; fi
if [ -e "$CAND" ]; then dirname -- "$CAND"; exit 0; fi
BIN="$(command -v fd || command -v fdfind || true)"
if [ -n "${BIN:-}" ]; then
  FIRST="$("$BIN" -H -c never -- "$Q" "$HOME_DIR" 2>/dev/null | head -n1)"
  if [ -n "$FIRST" ]; then [ -d "$FIRST" ] && printf "%%s\n" "$FIRST" || dirname -- "$FIRST"; exit 0; fi
fi
printf "%%s\n" "$HOME_DIR"
]],
		HOME,
		q
	)
	awful.spawn.easy_async_with_shell(sh, function(stdout)
		local target = stdout:match("([^\r\n]+)") or HOME
		awful.spawn({ "xdg-open", target }, false)
	end)
end

-- --- Web ---------------------------------------------------------------------
function P.web_search(query, ENGINE_FMT, BROWSER)
	local q = (query or ""):gsub("^%?+", ""):match("^%s*(.-)%s*$")
	if q == "" then
		return
	end
	local url = string.format(ENGINE_FMT or "https://duckduckgo.com/?q=%s", urlencode(q))
	awful.spawn({ BROWSER or "firefox", url }, false)
end

return P
