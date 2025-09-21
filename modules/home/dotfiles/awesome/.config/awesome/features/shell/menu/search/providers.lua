-- features/shell/menu/search/providers.lua
-- Kapselt alle Seiteneffekte: Lokalsuche (fd/fdfind + xdg-open) und Websuche.

local awful = require("awful")

local P = {}

-- nur intern hier:
local function open_with_file_manager(path)
	awful.spawn({ "xdg-open", path }, false)
end

-- nur intern hier:
local function urlencode(str)
	if not str then
		return ""
	end
	str = str:gsub("\n", " ")
	str = str:gsub("([^%w%-%_%.%~ ])", function(c)
		return string.format("%%%02X", string.byte(c))
	end)
	return str:gsub(" ", "+")
end

-- Lokalsuche (Ordner öffnen; Datei -> Parent; ~, abs/rel; fd/fdfind Fallback)
function P.local_search(query, HOME)
	HOME = HOME or (os.getenv("HOME") or "~")
	local q = (query or ""):match("^%s*(.-)%s*$")
	if q == "" then
		open_with_file_manager(HOME)
		return
	end

	local sh = string.format(
		[[
set -eu
HOME_DIR=%q
Q=%q

case "$Q" in
  "~"|"~/"*) Q="${Q/#\~/$HOME_DIR}";;
esac

if [ "${Q#/}" != "$Q" ]; then
  CAND="$Q"
else
  CAND="$HOME_DIR/$Q"
fi

if [ -d "$CAND" ]; then printf "%%s\n" "$CAND"; exit 0; fi
if [ -e "$CAND" ]; then dirname -- "$CAND"; exit 0; fi

BIN="$(command -v fd || command -v fdfind || true)"
if [ -n "${BIN:-}" ]; then
  FIRST="$("$BIN" -H -c never -- "$Q" "$HOME_DIR" 2>/dev/null | head -n1)"
  if [ -n "$FIRST" ]; then
    if [ -d "$FIRST" ]; then printf "%%s\n" "$FIRST"; else dirname -- "$FIRST"; fi
    exit 0
  fi
fi

printf "%%s\n" "$HOME_DIR"
]],
		HOME,
		q
	)

	awful.spawn.easy_async_with_shell(sh, function(stdout)
		local target = stdout:match("([^\r\n]+)")
		if not target or #target == 0 then
			target = HOME
		end
		open_with_file_manager(target)
	end)
end

-- Websuche
function P.web_search(query, ENGINE_FMT, BROWSER)
	local q = (query or ""):gsub("^%?+", ""):match("^%s*(.-)%s*$")
	if q == "" then
		return
	end
	local url = string.format(ENGINE_FMT or "https://duckduckgo.com/?q=%s", urlencode(q))
	awful.spawn({ BROWSER or "firefox", url }, false)
end

return P
