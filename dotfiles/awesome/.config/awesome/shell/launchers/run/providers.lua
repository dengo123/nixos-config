-- ~/.config/awesome/shell/launchers/run/providers.lua
local awful = require("awful")

local P = {}

local runtime = {
	ctx = {},
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function ctx()
	return runtime.ctx or {}
end

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

local function path_exists(path)
	if not path or path == "" then
		return false
	end

	local ok, _, code = os.rename(path, path)
	if ok then
		return true
	end

	return code == 13
end

local function is_directory(path)
	if not path or path == "" then
		return false
	end

	local probe = io.popen(string.format("test -d %s; printf %%s $?", sh_quote(path)))
	if not probe then
		return false
	end

	local rc = probe:read("*a")
	probe:close()

	return rc == "0"
end

local function is_regular_file(path)
	if not path or path == "" then
		return false
	end

	local probe = io.popen(string.format("test -f %s; printf %%s $?", sh_quote(path)))
	if not probe then
		return false
	end

	local rc = probe:read("*a")
	probe:close()

	return rc == "0"
end

local function lowercase_suffix(path)
	local suffix = tostring(path or ""):match("%.([^.]+)$")
	if not suffix then
		return nil
	end

	return suffix:lower()
end

local TEXT_SUFFIXES = {
	txt = true,
	md = true,
	rst = true,
	org = true,
	nix = true,
	lua = true,
	py = true,
	sh = true,
	bash = true,
	zsh = true,
	fish = true,
	js = true,
	ts = true,
	tsx = true,
	jsx = true,
	json = true,
	yaml = true,
	yml = true,
	toml = true,
	ini = true,
	conf = true,
	cfg = true,
	service = true,
	env = true,
	log = true,
	c = true,
	h = true,
	cpp = true,
	hpp = true,
	rs = true,
	go = true,
	java = true,
	kt = true,
	cs = true,
	rb = true,
	php = true,
	html = true,
	css = true,
	scss = true,
	xml = true,
	sql = true,
	csv = true,
	tex = true,
	el = true,
}

local function is_text_like_file(path)
	local suffix = lowercase_suffix(path)
	if suffix and TEXT_SUFFIXES[suffix] then
		return true
	end

	local probe = io.popen(string.format("file --mime-type -b %s 2>/dev/null", sh_quote(path)))
	if not probe then
		return false
	end

	local mime = trim(probe:read("*a") or "")
	probe:close()

	if mime:match("^text/") then
		return true
	end

	if
		mime == "application/json"
		or mime == "application/xml"
		or mime == "application/javascript"
		or mime == "application/x-shellscript"
	then
		return true
	end

	return false
end

local function open_with_files(target, apps_cfg)
	local files_cmd = apps_cfg.files or os.getenv("FILE_MANAGER") or "xdg-open"
	spawn_shell_bg(string.format("%s %q >/dev/null 2>&1 &", files_cmd, target))
end

local function open_with_editor(target, apps_cfg)
	local editor_cmd = apps_cfg.editor or os.getenv("EDITOR") or "nano"
	spawn_shell_bg(string.format("%s %q >/dev/null 2>&1 &", editor_cmd, target))
end

local function open_with_system(target)
	spawn_shell_bg(string.format("xdg-open %q >/dev/null 2>&1 &", target))
end

local function resolve_web_engine(engine)
	if type(engine) ~= "string" or engine == "" then
		return "https://www.google.com/search?q=%s"
	end

	local key = trim(engine):lower()

	local WEB_ENGINES = {
		google = "https://www.google.com/search?q=%s",
		duckduckgo = "https://duckduckgo.com/?q=%s",
		bing = "https://www.bing.com/search?q=%s",
		brave = "https://search.brave.com/search?q=%s",
		startpage = "https://www.startpage.com/do/search?q=%s",
		ecosia = "https://www.ecosia.org/search?q=%s",
		kagi = "https://kagi.com/search?q=%s",
	}

	return WEB_ENGINES[key] or WEB_ENGINES.google
end

-- =========================================================================
-- Init
-- =========================================================================

function P.init(args)
	runtime.ctx = (args and (args.ctx or args)) or {}
	return P
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

function P.local_open(query, home, local_cfg)
	local target = resolve_local_target(query, home)
	if not target or target == "" then
		return
	end

	local_cfg = local_cfg or {}
	local apps_cfg = local_cfg.apps or {}

	if not path_exists(target) then
		open_with_files(target, apps_cfg)
		return
	end

	if is_directory(target) then
		open_with_files(target, apps_cfg)
		return
	end

	if is_regular_file(target) then
		if is_text_like_file(target) then
			open_with_editor(target, apps_cfg)
			return
		end

		open_with_system(target)
		return
	end

	open_with_system(target)
end

-- =========================================================================
-- Web
-- =========================================================================

function P.web_open(query, engine_name, browser)
	local q = trim((query or ""):gsub("^%?+", ""))
	if q == "" then
		return
	end

	local engine_fmt = resolve_web_engine(engine_name)
	local url = string.format(engine_fmt, urlencode(q))

	awful.spawn({ browser or "firefox", url }, false)
end

-- =========================================================================
-- Compatibility
-- =========================================================================

P.run_search = P.run_execute
P.local_search = P.local_open
P.web_search = P.web_open

return P
