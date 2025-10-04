-- ~/.config/awesome/shell/launchers/run/controller.lua
-- Live-Fuzzy (autobest), Moduswechsel im Prompt (Ctrl+r/f/w), zoxide für Files.

local M = {}

-- ---------- fzy-ähnlicher Fuzzy-Score ----------
local function fuzzy_score(needle, hay)
	needle = (needle or ""):lower()
	hay = (hay or ""):lower()
	if needle == "" or hay == "" then
		return -math.huge
	end
	local n, h, score, streak = 1, 1, 0, 0
	while n <= #needle and h <= #hay do
		local cn, ch = needle:sub(n, n), hay:sub(h, h)
		if cn == ch then
			local bonus = 5
			local prev = (h == 1) and "/" or hay:sub(h - 1, h - 1)
			if prev == "/" or prev == "-" or prev == "_" or prev == " " or prev == "." then
				bonus = bonus + 4
			end
			streak = streak + 1
			score = score + bonus + math.min(streak, 3)
			n = n + 1
		else
			streak = 0
		end
		h = h + 1
	end
	if n <= #needle then
		return -math.huge
	end
	score = score + math.max(0, 6 - math.abs(#hay - #needle))
	return score
end

local function best_match(list, q)
	if not list or #list == 0 then
		return nil
	end
	if not q or q == "" then
		return list[1]
	end
	local best, bscore = nil, -math.huge
	for _, item in ipairs(list) do
		local s = fuzzy_score(q, item)
		if s > bscore then
			best, bscore = item, s
		end
	end
	return best
end

-- ---------- Kandidatenquellen ----------
local function split_path(p)
	local t = {}
	for seg in (p or ""):gmatch("([^:]+)") do
		t[#t + 1] = seg
	end
	return t
end

local function list_path_bins()
	local out, seen = {}, {}
	for _, dir in ipairs(split_path(os.getenv("PATH") or "")) do
		local f = io.popen(string.format("ls -1 %q 2>/dev/null", dir))
		if f then
			for name in f:lines() do
				if name and #name > 0 and not seen[name] then
					out[#out + 1] = name
					seen[name] = true
				end
			end
			f:close()
		end
	end
	table.sort(out)
	return out
end

local function list_desktop_names()
	local paths = {
		(os.getenv("XDG_DATA_HOME") or (os.getenv("HOME") .. "/.local/share")) .. "/applications",
		"/usr/share/applications",
		"/usr/local/share/applications",
		os.getenv("HOME") .. "/.local/share/flatpak/exports/share/applications",
		"/var/lib/flatpak/exports/share/applications",
		"/var/lib/snapd/desktop/applications",
	}
	local quoted = {}
	for _, p in ipairs(paths) do
		quoted[#quoted + 1] = string.format("%q", p)
	end
	local cmd = "find "
		.. table.concat(quoted, " ")
		.. " -type f -name '*.desktop' 2>/dev/null | sed 's#.*/##;s#\\.desktop$##' | sort -u"
	local out, f = {}, io.popen(cmd)
	if f then
		for name in f:lines() do
			out[#out + 1] = name
		end
		f:close()
	end
	return out
end

local function list_home_entries()
	local home = os.getenv("HOME") or "~"
	local out, f = {}, io.popen(string.format("cd %q && ls -a 2>/dev/null", home))
	if f then
		for name in f:lines() do
			out[#out + 1] = name
		end
		f:close()
	end
	table.sort(out)
	return out
end

local function zoxide_list_all()
	local out, f = {}, io.popen("zoxide query -l 2>/dev/null")
	if f then
		for line in f:lines() do
			if line and #line > 0 then
				out[#out + 1] = line
			end
		end
		f:close()
	end
	return out
end

-- ---------- Controller ----------
function M.new(ctx)
	assert(ctx and ctx.parts and ctx.parts.textbox, "run.controller: ctx.parts.textbox required")

	local awful, gears = ctx.awful, ctx.gears
	local textbox = ctx.parts.textbox
	local prefix_lbl = ctx.parts.prefix_lbl
	local inner_margin = ctx.parts.inner_margin
	local bg_box = ctx.parts.bg_box
	local width_ctl = ctx.parts.width_ctl

	local WIDTH_EXP = (ctx.sizes and ctx.sizes.width_expanded) or 400
	local BG_ACTIVE = (ctx.colors and ctx.colors.bg_active) or "#FFFFFF"
	local FG_ACTIVE = (ctx.colors and ctx.colors.fg_active) or "#000000"
	local CURSOR_BG = (ctx.colors and ctx.colors.cursor_bg) or "#00000000"
	local CURSOR_FG = (ctx.colors and ctx.colors.cursor_fg) or FG_ACTIVE

	local PAD_L = (ctx.layout and ctx.layout.left) or 12
	local PAD_R = (ctx.layout and ctx.layout.right) or 12
	local PAD_T = (ctx.layout and ctx.layout.top) or 8
	local PAD_B = (ctx.layout and ctx.layout.bottom) or 8

	local PREFIX_RUN = (ctx.prefixes and ctx.prefixes.run_mode) or ""
	local PREFIX_LOCAL = (ctx.prefixes and ctx.prefixes.local_mode) or "/"
	local PREFIX_WEB = (ctx.prefixes and ctx.prefixes.web_mode) or "?"

	local Providers = ctx.providers or {}
	local HOME = ctx.home
	local BROWSER = ctx.web and ctx.web.browser
	local ENGINE = ctx.web and ctx.web.engine

	local state = {
		prompt_running = false,
		mode = "run",
		cache = { apps = false, home = false, zox = false },
		lock_autofill = false, -- verhindert Rekursion beim Setzen von Text
		min_len_for_fuzzy = 1, -- erst ab X Zeichen automatisch vervollständigen
	}

	local function try(fn, ...)
		if type(fn) ~= "function" then
			return false
		end
		local ok, _ = pcall(fn, ...)
		return ok
	end

	local function apply_active_style()
		if bg_box then
			bg_box.bg = BG_ACTIVE
		end
		if inner_margin then
			inner_margin.left, inner_margin.right = PAD_L, PAD_R
			inner_margin.top, inner_margin.bottom = PAD_T, PAD_B
		end
		if width_ctl then
			width_ctl.width = WIDTH_EXP
			width_ctl:emit_signal("widget::layout_changed")
		end
		try(function()
			textbox.bg = BG_ACTIVE
		end)
		try(function()
			textbox.fg = FG_ACTIVE
		end)
		try(function()
			textbox.bg_cursor = CURSOR_BG
		end)
		try(function()
			textbox.fg_cursor = CURSOR_FG
		end)
	end

	local function ensure_caches()
		if not state.cache.apps then
			local names = list_desktop_names()
			local bins = list_path_bins()
			local seen, union = {}, {}
			for _, v in ipairs(names) do
				if not seen[v] then
					union[#union + 1] = v
					seen[v] = true
				end
			end
			for _, v in ipairs(bins) do
				if not seen[v] then
					union[#union + 1] = v
					seen[v] = true
				end
			end
			table.sort(union)
			state.cache.apps = union
		end
		if not state.cache.home then
			state.cache.home = list_home_entries()
		end
		if not state.cache.zox then
			state.cache.zox = zoxide_list_all()
		end
	end

	local function set_mode(new_mode)
		state.mode = new_mode
		if prefix_lbl then
			prefix_lbl.text = (new_mode == "local") and PREFIX_LOCAL or (new_mode == "web") and PREFIX_WEB or PREFIX_RUN
		end
		try(function()
			textbox:set_text("")
		end)
	end

	-- Live-Autofill: berechnet besten Treffer und setzt Textbox
	local function update_autofill()
		if state.lock_autofill then
			return
		end
		local cur = textbox and (textbox.text or "")
		cur = (cur or ""):match("^%s*(.-)%s*$")

		if (cur or "") == "" or #cur < state.min_len_for_fuzzy then
			return
		end

		local list
		if state.mode == "local" then
			-- Union zoxide + $HOME-Einträge
			local seen, union = {}, {}
			for _, v in ipairs(state.cache.zox or {}) do
				if not seen[v] then
					union[#union + 1] = v
					seen[v] = true
				end
			end
			for _, v in ipairs(state.cache.home or {}) do
				if not seen[v] then
					union[#union + 1] = v
					seen[v] = true
				end
			end
			list = union
		elseif state.mode == "web" then
			list = {} -- optional: History
		else
			list = state.cache.apps or {}
		end

		local pick = best_match(list, cur)
		if pick and pick ~= cur then
			state.lock_autofill = true
			try(function()
				textbox:set_text(pick)
				-- Cursor ans Ende
				if textbox and textbox.cursor then
					textbox.cursor = #pick + 1
				end
			end)
			state.lock_autofill = false
		end
	end

	local function clear_text()
		try(function()
			textbox:set_text("")
		end)
		if prefix_lbl then
			prefix_lbl.text = ""
		end
	end

	local function stop_prompt()
		if state.prompt_running then
			state.prompt_running = false
		end
		try(awful.keygrabber.stop)
		gears.timer.delayed_call(function()
			try(awful.keygrabber.stop)
		end)
		clear_text()
	end

	local function dispatch(mode, q)
		if mode == "local" then
			-- erst versuchen: zoxide
			local sh = [=[#!/bin/sh
set -eu
Q="$1"
if command -v zoxide >/dev/null 2>&1; then
  TARGET="$(zoxide query -- "$Q" 2>/dev/null || true)"
  if [ -n "${TARGET:-}" ] && [ -d "$TARGET" ]; then
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
    exit 0
  fi
fi
exit 42
]=]
			local ok = awful.spawn.with_line_callback({ "sh", "-c", sh, "--", q or "" }, {
				exit = function(code)
					if tonumber(code) == 42 then
						if Providers.local_search then
							Providers.local_search(q, HOME)
						end
					end
				end,
			})
			if not ok and Providers.local_search then
				Providers.local_search(q, HOME)
			end
		elseif mode == "web" then
			if Providers.web_search then
				Providers.web_search(q, ENGINE, BROWSER)
			end
		else
			if Providers.run_search then
				Providers.run_search(q)
			end
		end
	end

	local function run_prompt()
		state.prompt_running = true
		apply_active_style()
		ensure_caches()

		-- Wichtig: live-Autofill über keypressed-Callback; completion_callback bleibt ungenutzt
		awful.prompt.run({
			prompt = "",
			textbox = textbox,
			history_path = nil,

			exe_callback = function(q)
				q = (q or ""):match("^%s*(.-)%s*$")
				dispatch(state.mode, q)
				stop_prompt()
				if ctx.hide_menu_popup then
					ctx.hide_menu_popup()
				end
			end,

			done_callback = function()
				stop_prompt()
				if ctx.hide_menu_popup then
					ctx.hide_menu_popup()
				end
			end,

			keypressed_callback = function(mod, key)
				-- Escape → schließen
				if (not mod or #mod == 0) and key == "Escape" then
					stop_prompt()
					if ctx.hide_menu_popup then
						ctx.hide_menu_popup()
					end
					return true
				end

				-- Moduswechsel: Ctrl+r / Ctrl+f / Ctrl+w
				local ctrl = false
				for _, m in ipairs(mod or {}) do
					if m == "Control" then
						ctrl = true
						break
					end
				end
				if ctrl then
					if key == "r" then
						set_mode("run")
						return true
					end
					if key == "f" then
						set_mode("local")
						return true
					end
					if key == "w" then
						set_mode("web")
						return true
					end
				end

				-- Nach normalen Tastenanschlägen live-Fuzzy anwenden (asynchron, damit Prompt erst Text setzt)
				gears.timer.delayed_call(function()
					update_autofill()
				end)
				return false
			end,
		})
	end

	-- Public API
	local api = {
		init = function()
			apply_active_style()
			ensure_caches()
		end,
		focus_run = function()
			set_mode("run")
			run_prompt()
		end,
		focus_local = function()
			set_mode("local")
			run_prompt()
		end,
		focus_web = function()
			set_mode("web")
			run_prompt()
		end,
		cancel = function()
			stop_prompt()
		end,
		is_active = function()
			return state.prompt_running == true
		end,
		set_engine = function(s)
			ENGINE = s or ENGINE
		end,
		set_browser = function(b)
			BROWSER = b or BROWSER
		end,
	}

	return api
end

return M
