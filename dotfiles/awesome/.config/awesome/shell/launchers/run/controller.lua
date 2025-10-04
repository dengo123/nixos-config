-- ~/.config/awesome/shell/launchers/run/controller.lua
-- Verantwortlich für: Prompt-Lifecycle & Dispatch zu den Providern.
-- Annahmen:
--   ctx.parts   = { textbox, prefix_lbl, inner_margin, bg_box, width_ctl }
--   ctx.sizes   = { width_expanded, width_collapsed, height? }
--   ctx.colors  = { bg_active, bg_collapsed?, fg_active, cursor_bg, cursor_fg }
--   ctx.layout  = { left, right, top, bottom }
--   ctx.prefixes= { run_mode, local_mode, web_mode }
--   ctx.providers = { run_search(q), local_search(q, HOME), web_search(q, ENGINE_FMT, BROWSER) }
--   ctx.web     = { browser, engine }
--   ctx.home    = $HOME
--   ctx.awful, ctx.gears
--   ctx.hide_menu_popup()  -- optional: schließt den umgebenden Popup

local M = {}

local function try(fn, ...)
	if type(fn) ~= "function" then
		return false
	end
	local ok, _ = pcall(fn, ...)
	return ok
end

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
		mode = "run", -- default: Apps
	}

	local function apply_active_style()
		-- feste, „expandte“ Optik (keine Collapse-Logik mehr)
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
		-- Textbox-Farben
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
		-- zweimal stoppen ist ok (Workaround für rennende Grabber)
		try(awful.keygrabber.stop)
		gears.timer.delayed_call(function()
			try(awful.keygrabber.stop)
		end)
		clear_text()
	end

	local function dispatch(mode, q)
		if mode == "local" then
			if Providers.local_search then
				Providers.local_search(q, HOME)
			end
		elseif mode == "web" then
			if Providers.web_search then
				Providers.web_search(q, ENGINE, BROWSER)
			end
		else -- "run" (Apps)
			if Providers.run_search then
				Providers.run_search(q)
			end
		end
	end

	local function run_prompt(mode)
		state.mode = mode or "run"
		if prefix_lbl then
			prefix_lbl.text = (state.mode == "local") and PREFIX_LOCAL
				or (state.mode == "web") and PREFIX_WEB
				or PREFIX_RUN
		end

		gears.timer.delayed_call(function()
			state.prompt_running = true
			apply_active_style()

			awful.prompt.run({
				prompt = "",
				textbox = textbox,
				history_path = nil,
				completion_callback = nil,

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
					if (not mod or #mod == 0) and key == "Escape" then
						stop_prompt()
						if ctx.hide_menu_popup then
							ctx.hide_menu_popup()
						end
						return true
					end
					return false
				end,
			})
		end)
	end

	-- Public API
	local api = {
		-- Styling einmal setzen (fixed-Layout)
		init = function()
			apply_active_style()
		end,
		-- Modi
		focus_run = function()
			run_prompt("run")
		end,
		focus_local = function()
			run_prompt("local")
		end,
		focus_web = function()
			run_prompt("web")
		end,
		-- Lifecycle
		cancel = function()
			stop_prompt()
		end,
		is_active = function()
			return state.prompt_running
		end,
		-- Runtime-Config (optional)
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
