-- ~/.config/awesome/shell/launchers/run/providers.lua
local awful = require("awful")

local P = {}

-- =========================================================================
-- Helpers
-- =========================================================================

local function urlencode(s)
	if not s then
		return ""
	end

	s = s:gsub("\n", " "):gsub("([^%w%-%_%.%~ ])", function(c)
		return string.format("%%%02X", string.byte(c))
	end)

	return s:gsub(" ", "+")
end

local function sh_quote(s)
	s = tostring(s or "")
	return "'" .. s:gsub("'", "'\"'\"'") .. "'"
end

local function trim(s)
	return (s or ""):match("^%s*(.-)%s*$")
end

local function resolve_home(home)
	home = home or os.getenv("HOME")
	if type(home) == "string" and #home > 0 then
		return home
	end
	return nil
end

local function resolve_local_target(query, home)
	local q = trim(query)
	local resolved_home = resolve_home(home)

	if q == "" or q == "~" then
		return resolved_home
	end

	if q:sub(1, 1) == "~" and resolved_home then
		return q:gsub("^~", resolved_home)
	end

	if q:sub(1, 1) == "/" then
		return q
	end

	if resolved_home then
		return resolved_home .. "/" .. q
	end

	return q
end

local function spawn_shell_bg(cmd)
	awful.spawn.with_shell(cmd)
end

-- =========================================================================
-- Run
-- =========================================================================

function P.run_execute(query)
	local q = trim(query)
	if q == "" then
		return
	end

	local tmpl = [=[#!/bin/sh
set -eu
Q=__Q__
paths="${XDG_DATA_HOME:-$HOME/.local/share}/applications \
/etc/xdg/applications \
/run/current-system/sw/share/applications \
/usr/share/applications /usr/local/share/applications \
$HOME/.local/share/flatpak/exports/share/applications \
/var/lib/flatpak/exports/share/applications \
/var/lib/snapd/desktop/applications"

for d in ${XDG_DATA_DIRS:-}; do
  paths="$paths $d/applications"
done

list_files() {
  if command -v fd >/dev/null 2>&1; then
    fd -t f -e desktop . $paths
  elif command -v fdfind >/dev/null 2>&1; then
    fdfind -t f -e desktop . $paths
  else
    find $paths -type f -name '*.desktop' 2>/dev/null
  fi
}

pick="$(list_files | awk -v q="$Q" 'BEGIN{IGNORECASE=1}{
  f=$0; sub(/.*\//,"",f);
  if (index(f,q)) { print $0; exit }
}')"

if [ -z "${pick:-}" ] && command -v grep >/dev/null 2>&1; then
  pick="$(list_files | xargs -r grep -IlF 'Name=' 2>/dev/null | xargs -r grep -IlF "$Q" 2>/dev/null | head -n1 || true)"
  if [ -z "${pick:-}" ]; then
    pick="$(list_files | xargs -r grep -IlF 'Exec=' 2>/dev/null | xargs -r grep -IlF "$Q" 2>/dev/null | head -n1 || true)"
  fi
fi

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

if command -v gtk-launch >/dev/null 2>&1; then
  gtk-launch "$app" >/dev/null 2>&1 & launched=1
fi

if [ -z "${launched:-}" ] && command -v dex >/dev/null 2>&1; then
  dex "$pick" >/dev/null 2>&1 & launched=1
fi

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

if [ -z "${launched:-}" ]; then
  if command -v "$Q" >/dev/null 2>&1; then
    nohup "$Q" >/dev/null 2>&1 &
  else
    nohup sh -c "$Q" >/dev/null 2>&1 || true &
  fi
fi
]=]

	spawn_shell_bg(tmpl:gsub("__Q__", sh_quote(q)))
end

-- =========================================================================
-- Local
-- =========================================================================

function P.local_open(query, home)
	local target = resolve_local_target(query, home)
	if not target or target == "" then
		return
	end

	local files = os.getenv("FILE_MANAGER")
	if not files or files == "" then
		files = "xdg-open"
	end

	spawn_shell_bg(string.format("%s %q >/dev/null 2>&1 &", files, target))
end

-- =========================================================================
-- Web
-- =========================================================================

function P.web_open(query, engine_fmt, browser)
	local q = trim((query or ""):gsub("^%?+", ""))
	if q == "" then
		return
	end

	local url = string.format(engine_fmt or "https://duckduckgo.com/?q=%s", urlencode(q))
	awful.spawn({ browser or "firefox", url }, false)
end

-- =========================================================================
-- Compatibility
-- =========================================================================

P.run_search = P.run_execute
P.local_search = P.local_open
P.web_search = P.web_open

return P
