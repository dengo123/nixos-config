-- features/shell/menu/search/backend.lua
-- Backends: lokale Suche, Websuche, History-/Completion-Helfer

local awful = require("awful")

local M = {}

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

-- Lokalsuche: Ordner öffnen; bei Datei -> Parent; ~, abs/rel; fd/fdfind Fallback
function M.local_search(query, home, open_fn)
	local HOME = home or (os.getenv("HOME") or "~")
	local q = (query or ""):match("^%s*(.-)%s*$")
	if q == "" then
		if open_fn then
			open_fn(HOME)
		end
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
		local target = stdout:match("([^\r\n]+)") or HOME
		if open_fn then
			open_fn(target)
		end
	end)
end

-- Websuche: öffnet Browser
function M.web_search(query, browser, engine_fmt)
	local q = (query or ""):gsub("^%?+", ""):match("^%s*(.-)%s*$")
	if q == "" then
		return
	end
	local url = string.format(engine_fmt or "https://duckduckgo.com/?q=%s", urlencode(q))
	awful.spawn({ browser or "firefox", url }, false)
end

-- History laden (einfaches Zeilenformat)
function M.history_load(path, max_lines)
	local out = {}
	local f = path and io.open(path, "r") or nil
	if not f then
		return out
	end
	for line in f:lines() do
		if line and line ~= "" then
			out[#out + 1] = line
			if max_lines and #out >= max_lines then
				break
			end
		end
	end
	f:close()
	return out
end

-- Completion aus History (prefix, case-insensitive), sortiert: kürzere zuerst
function M.complete_from_history(prefix, hist, limit)
	prefix = (prefix or ""):match("^%s*(.-)%s*$")
	if prefix == "" then
		return {}
	end
	local pfx = prefix:lower()
	local res = {}
	for _, line in ipairs(hist or {}) do
		if line:sub(1, #prefix):lower() == pfx then
			res[#res + 1] = line
			if limit and #res >= limit then
				break
			end
		end
	end
	table.sort(res, function(a, b)
		return #a < #b
	end)
	return res
end

return M
