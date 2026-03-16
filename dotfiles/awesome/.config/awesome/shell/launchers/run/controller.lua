-- ~/.config/awesome/shell/launchers/run/controller.lua
local M = {}

function M.new(ctx)
	assert(ctx and ctx.parts and ctx.parts.textbox, "run.controller: ctx.parts.textbox required")

	-- =========================================================================
	-- Context
	-- =========================================================================

	local awful = ctx.awful
	local gears = ctx.gears
	local Providers = ctx.providers or {}
	local Complete = ctx.complete

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

	local PREFIX_RUN = (ctx.prefixes and ctx.prefixes.run_mode) or "run:"
	local PREFIX_LOCAL = (ctx.prefixes and ctx.prefixes.local_mode) or "files:"
	local PREFIX_WEB = (ctx.prefixes and (ctx.prefixes.search_mode or ctx.prefixes.web_mode)) or "search:"

	local HOME = ctx.home
	local BROWSER = ctx.web and ctx.web.browser
	local ENGINE = ctx.web and ctx.web.engine

	local state = {
		prompt_running = false,
		mode = "run",
		min_len_for_fuzzy = 1,
	}

	-- =========================================================================
	-- Helpers
	-- =========================================================================

	local function try(fn, ...)
		if type(fn) ~= "function" then
			return false
		end
		local ok = pcall(fn, ...)
		return ok
	end

	local function current_text()
		return (textbox and textbox.get_text and textbox:get_text() or textbox.text or ""):match("^%s*(.-)%s*$")
	end

	local function set_text(s)
		s = s or ""
		try(function()
			textbox:set_text(s)
		end)
		if textbox and textbox.cursor then
			textbox.cursor = #s + 1
		end
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

	local function set_prefix()
		if not prefix_lbl then
			return
		end

		if state.mode == "local" then
			prefix_lbl.text = PREFIX_LOCAL
		elseif state.mode == "web" then
			prefix_lbl.text = PREFIX_WEB
		else
			prefix_lbl.text = PREFIX_RUN
		end
	end

	local function set_mode(new_mode)
		state.mode = (new_mode == "files") and "local" or new_mode
		set_prefix()
	end

	local function cycle_mode()
		if state.mode == "run" then
			set_mode("local")
		elseif state.mode == "local" then
			set_mode("web")
		else
			set_mode("run")
		end
	end

	local function update_autofill()
		if not Complete then
			return
		end

		local cur = current_text()
		if cur == "" or #cur < state.min_len_for_fuzzy then
			return
		end

		Complete.ensure()
		local list = Complete.candidates(state.mode)
		local pick = Complete.best(list, cur)

		if pick and pick ~= cur then
			set_text(pick)
		end
	end

	local function clear_text()
		set_text("")
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
		set_prefix()
	end

	local function dispatch(q)
		q = (q or ""):match("^%s*(.-)%s*$")

		if state.mode == "local" then
			if Providers.local_open then
				Providers.local_open(q, HOME)
			elseif Providers.local_search then
				Providers.local_search(q, HOME)
			end
		elseif state.mode == "web" then
			if Providers.web_open then
				Providers.web_open(q, ENGINE, BROWSER)
			elseif Providers.web_search then
				Providers.web_search(q, ENGINE, BROWSER)
			end
		else
			if Providers.run_execute then
				Providers.run_execute(q)
			elseif Providers.run_search then
				Providers.run_search(q)
			end
		end
	end

	local function switch_mode_keep_prompt(new_mode)
		set_mode(new_mode)
		set_prefix()
		gears.timer.delayed_call(update_autofill)
	end

	local function submit_current()
		local cur = current_text()
		local final = cur

		if cur ~= "" and Complete then
			Complete.ensure()
			local list = Complete.candidates(state.mode)
			local pick = Complete.best(list, cur)
			if pick and pick ~= "" then
				final = pick
			end
		end

		dispatch(final)
		stop_prompt()

		if ctx.hide_menu_popup then
			ctx.hide_menu_popup()
		end
	end

	local function run_prompt()
		state.prompt_running = true
		apply_active_style()
		set_prefix()

		awful.prompt.run({
			prompt = "",
			textbox = textbox,
			history_path = nil,

			exe_callback = function(q)
				if q ~= nil then
					set_text((q or ""):match("^%s*(.-)%s*$"))
				end
				submit_current()
			end,

			done_callback = function()
				stop_prompt()
				if ctx.hide_menu_popup then
					ctx.hide_menu_popup()
				end
			end,

			keypressed_callback = function(mod, key)
				mod = mod or {}

				if #mod == 0 and key == "Escape" then
					stop_prompt()
					if ctx.hide_menu_popup then
						ctx.hide_menu_popup()
					end
					return true
				end

				if #mod == 0 and key == "Tab" then
					cycle_mode()
					gears.timer.delayed_call(update_autofill)
					return true
				end

				gears.timer.delayed_call(update_autofill)
				return false
			end,
		})
	end

	-- =========================================================================
	-- Public API
	-- =========================================================================

	local api = {
		init = function()
			apply_active_style()
			set_prefix()
		end,

		focus_run = function()
			if state.prompt_running then
				switch_mode_keep_prompt("run")
				return
			end
			set_mode("run")
			run_prompt()
		end,

		focus_local = function()
			if state.prompt_running then
				switch_mode_keep_prompt("local")
				return
			end
			set_mode("local")
			run_prompt()
		end,

		focus_web = function()
			if state.prompt_running then
				switch_mode_keep_prompt("web")
				return
			end
			set_mode("web")
			run_prompt()
		end,

		rotate_mode = function()
			if state.prompt_running then
				cycle_mode()
				gears.timer.delayed_call(update_autofill)
				return
			end
			cycle_mode()
			run_prompt()
		end,

		submit = function()
			if not state.prompt_running then
				return
			end
			submit_current()
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
