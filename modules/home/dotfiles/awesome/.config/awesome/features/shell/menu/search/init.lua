-- features/shell/menu/search/init.lua
local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")

local M = {}

local function hide_menu_popup()
	local api = rawget(_G, "__menu_api")
	if api and type(api.hide) == "function" then
		api.hide()
	else
		-- Fallback, falls du lieber mit Signals arbeitest:
		awesome.emit_signal("menu::hide")
	end
end

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

-- kleine Utility: defensive call
local function try(fn)
	return function(...)
		local ok, _ = pcall(fn, ...)
		return ok
	end
end

function M.new(opts)
	opts = opts or {}
	local FOOT_H = opts.footer_h or 48
	local H = math.max(16, math.floor(FOOT_H / 3 + 0.5))
	local WIDTH_EXP = opts.width_expanded or 200
	local WIDTH_COL = opts.width_collapsed or WIDTH_EXP
	local HOME = os.getenv("HOME") or "~"

	local colors = opts.colors or {}
	local BG_ACTIVE = colors.bg or "#FFFFFF"
	local FG_ACTIVE = colors.fg or "#000000"
	local BG_COLLAPSED = colors.bg_collapsed or "#00000000"

	local web = opts.web or {}
	local BROWSER = web.browser or "firefox"
	local ENGINE_FMT = web.engine or "https://duckduckgo.com/?q=%s"

	-- Prompt + Textbox
	local prompt = awful.widget.prompt({})
	local textbox = prompt.widget
	if textbox.set_align then
		textbox:set_align("left")
	end
	if textbox.set_valign then
		textbox:set_valign("center")
	end
	if textbox.set_text then
		textbox:set_text("")
	end
	textbox.bg = BG_ACTIVE
	textbox.fg = FG_ACTIVE
	textbox.bg_cursor = "#00000000"
	textbox.fg_cursor = FG_ACTIVE

	-- fixes Präfix-Label (nicht editierbar)
	local prefix_lbl = wibox.widget({
		text = "",
		font = textbox.font or nil,
		align = "left",
		valign = "center",
		widget = wibox.widget.textbox,
	})

	-- Innenlayout
	local inner_margin = wibox.widget({
		{
			prefix_lbl,
			textbox,
			spacing = 8,
			layout = wibox.layout.fixed.horizontal,
		},
		left = 10,
		right = 10,
		top = 4,
		bottom = 4,
		widget = wibox.container.margin,
	})

	local bg_box = wibox.widget({
		inner_margin,
		bg = BG_ACTIVE,
		fg = FG_ACTIVE,
		widget = wibox.container.background,
	})

	local height_ctl = wibox.widget({
		bg_box,
		strategy = "exact",
		height = H,
		widget = wibox.container.constraint,
	})

	local vcenter = wibox.widget({
		height_ctl,
		valign = "center",
		widget = wibox.container.place,
	})

	local width_ctl = wibox.widget({
		vcenter,
		strategy = "exact",
		width = WIDTH_EXP,
		widget = wibox.container.constraint,
	})

	local hleft = wibox.widget({
		width_ctl,
		halign = "left",
		widget = wibox.container.place,
	})

	local widget = wibox.widget({
		hleft,
		layout = wibox.layout.fixed.horizontal,
	})

	-- Status
	local state = {
		prompt_running = false,
		collapsed = true,
		saved_root_buttons = nil,
		ignore_next_root_click = false,
		mode = "local", -- "local" | "web"
	}

	-- Öffnen im Filemanager
	local function open_with_file_manager(path)
		awful.spawn({ "xdg-open", path }, false)
	end

	-- Lokalsuche (Ordner öffnen; Datei -> Parent; ~, abs/rel; fd/fdfind Fallback)
	local function run_local_search(query)
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
	local function run_web_search(query)
		local q = (query or ""):gsub("^%?+", ""):match("^%s*(.-)%s*$")
		if q == "" then
			return
		end
		local url = string.format(ENGINE_FMT, urlencode(q))
		awful.spawn({ BROWSER, url }, false)
	end

	-- Root-Click Watcher
	local function detach_root_click_watcher()
		if state.saved_root_buttons then
			root.buttons(state.saved_root_buttons)
			state.saved_root_buttons = nil
		end
	end

	local function attach_root_click_watcher()
		if not state.saved_root_buttons then
			state.saved_root_buttons = root.buttons()
		end
		root.buttons(gears.table.join(awful.button({}, 1, function()
			if state.ignore_next_root_click then
				return
			end
			if state.prompt_running then
				try(awful.keygrabber.stop)()
			end
			state.prompt_running = false
			bg_box.bg = BG_COLLAPSED
			inner_margin.left, inner_margin.right, inner_margin.top, inner_margin.bottom = 0, 0, 0, 0
			bg_box.forced_width = WIDTH_COL
			width_ctl.width = WIDTH_COL
			width_ctl:emit_signal("widget::layout_changed")
			state.collapsed = true
			detach_root_click_watcher()
		end)))
	end

	local function apply_collapsed_style()
		state.collapsed = true
		bg_box.bg = BG_COLLAPSED
		inner_margin.left, inner_margin.right, inner_margin.top, inner_margin.bottom = 0, 0, 0, 0
		bg_box.forced_width = WIDTH_COL
		width_ctl.width = WIDTH_COL
		width_ctl:emit_signal("widget::layout_changed")
	end

	local function apply_expanded_style()
		state.collapsed = false
		bg_box.bg = BG_ACTIVE
		inner_margin.left, inner_margin.right, inner_margin.top, inner_margin.bottom = 10, 10, 4, 4
		bg_box.forced_width = WIDTH_EXP
		width_ctl.width = WIDTH_EXP
		width_ctl:emit_signal("widget::layout_changed")
	end

	local function stop_prompt()
		if state.prompt_running then
			state.prompt_running = false
		end
		try(awful.keygrabber.stop)()
		gears.timer.delayed_call(function()
			try(awful.keygrabber.stop)()
		end)
		try(function()
			if textbox.set_text then
				textbox:set_text("")
			end
		end)()
		prefix_lbl.text = ""
		detach_root_click_watcher()
	end

	local function collapse()
		stop_prompt()
		apply_collapsed_style()
	end

	local function run_prompt(mode)
		state.mode = mode or "local"
		prefix_lbl.text = (state.mode == "local") and "/" or "?"

		gears.timer.delayed_call(function()
			state.prompt_running = true
			try(function()
				prompt.bg = BG_ACTIVE
			end)()
			try(function()
				prompt.fg = FG_ACTIVE
			end)()
			try(function()
				prompt.bg_cursor = "#00000000"
			end)()
			try(function()
				prompt.fg_cursor = FG_ACTIVE
			end)()

			awful.prompt.run({
				prompt = "",
				textbox = textbox,
				history_path = nil,
				completion_callback = nil,

				exe_callback = function(q)
					q = (q or ""):match("^%s*(.-)%s*$")
					if state.mode == "local" then
						run_local_search(q)
					else
						run_web_search(q)
					end
					collapse()
				end,

				done_callback = function()
					collapse()
				end,

				keypressed_callback = function(mod, key)
					if (not mod or #mod == 0) and key == "Escape" then
						collapse()
						hide_menu_popup()
						return true
					end
					return false
				end,
			})
		end)
	end

	local function expand_and_start(mode)
		stop_prompt()
		apply_expanded_style()
		attach_root_click_watcher()
		run_prompt(mode or "local")
	end

	local function on_left_click()
		state.ignore_next_root_click = true
		gears.timer.delayed_call(function()
			state.ignore_next_root_click = false
		end)
		expand_and_start("local")
	end

	local function on_right_click()
		state.ignore_next_root_click = true
		gears.timer.delayed_call(function()
			state.ignore_next_root_click = false
		end)
		expand_and_start("web")
	end

	widget:buttons(gears.table.join(awful.button({}, 1, on_left_click), awful.button({}, 3, on_right_click)))

	-- initial: collapsed
	apply_collapsed_style()

	-- Public API
	local api = {
		focus_local = function()
			expand_and_start("local")
		end,
		focus_web = function()
			expand_and_start("web")
		end,
		cancel = function()
			collapse()
		end,
		is_active = function()
			return state.prompt_running
		end,
		is_collapsed = function()
			return state.collapsed
		end,
		set_engine = function(s)
			ENGINE_FMT = s or ENGINE_FMT
		end,
		set_browser = function(b)
			BROWSER = b or BROWSER
		end,
	}

	return { widget = widget, api = api }
end

return M
