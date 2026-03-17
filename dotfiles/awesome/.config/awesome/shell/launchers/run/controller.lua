-- ~/.config/awesome/shell/launchers/run/controller.lua
local M = {}

function M.new(ctx)
	assert(ctx and ctx.parts and ctx.parts.textbox, "run.controller: ctx.parts.textbox required")

	-- =========================================================================
	-- Config
	-- =========================================================================

	local awful = assert(ctx.awful, "run.controller: ctx.awful required")
	local gears = assert(ctx.gears, "run.controller: ctx.gears required")
	local Providers = ctx.providers or {}
	local Complete = ctx.complete

	local parts = assert(ctx.parts, "run.controller: ctx.parts required")
	local view = assert(ctx.view, "run.controller: ctx.view required")

	local textbox = parts.textbox

	local sizes = assert(ctx.sizes, "run.controller: ctx.sizes required")
	local colors = assert(ctx.colors, "run.controller: ctx.colors required")
	local layout = assert(ctx.layout, "run.controller: ctx.layout required")
	local prefixes = assert(ctx.prefixes, "run.controller: ctx.prefixes required")

	local width_expanded = assert(tonumber(sizes.width_expanded), "run.controller: sizes.width_expanded required")

	local bg_active = assert(colors.bg_active, "run.controller: colors.bg_active required")
	local fg_active = assert(colors.fg_active, "run.controller: colors.fg_active required")
	local cursor_bg = assert(colors.cursor_bg, "run.controller: colors.cursor_bg required")
	local cursor_fg = assert(colors.cursor_fg, "run.controller: colors.cursor_fg required")

	local pad_l = assert(tonumber(layout.left), "run.controller: layout.left required")
	local pad_r = assert(tonumber(layout.right), "run.controller: layout.right required")
	local pad_t = assert(tonumber(layout.top), "run.controller: layout.top required")
	local pad_b = assert(tonumber(layout.bottom), "run.controller: layout.bottom required")

	local prefix_run = assert(prefixes.run_mode, "run.controller: prefixes.run_mode required")
	local prefix_local = assert(prefixes.local_mode, "run.controller: prefixes.local_mode required")
	local prefix_web = assert(prefixes.web_mode, "run.controller: prefixes.web_mode required")

	local home = ctx.home
	local browser = ctx.web and ctx.web.browser
	local engine = ctx.web and ctx.web.engine

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
		view.apply_active_style({
			bg_active = bg_active,
			fg_active = fg_active,
			left = pad_l,
			right = pad_r,
			top = pad_t,
			bottom = pad_b,
			width_expanded = width_expanded,
		})

		try(function()
			textbox.bg = bg_active
		end)

		try(function()
			textbox.fg = fg_active
		end)

		try(function()
			textbox.bg_cursor = cursor_bg
		end)

		try(function()
			textbox.fg_cursor = cursor_fg
		end)
	end

	local function set_prefix()
		if state.mode == "local" then
			view.set_prefix(prefix_local)
		elseif state.mode == "web" then
			view.set_prefix(prefix_web)
		else
			view.set_prefix(prefix_run)
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
		local best = Complete.best(list, cur)

		if best and best ~= cur then
			set_text(best)
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
				Providers.local_open(q, home)
			elseif Providers.local_search then
				Providers.local_search(q, home)
			end
		elseif state.mode == "web" then
			if Providers.web_open then
				Providers.web_open(q, engine, browser)
			elseif Providers.web_search then
				Providers.web_search(q, engine, browser)
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
		gears.timer.delayed_call(update_autofill)
	end

	local function submit_current()
		local cur = current_text()
		local final = cur

		if cur ~= "" and Complete then
			Complete.ensure()
			local list = Complete.candidates(state.mode)
			local best = Complete.best(list, cur)

			if best and best ~= "" then
				final = best
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
			engine = s or engine
		end,

		set_browser = function(b)
			browser = b or browser
		end,
	}

	return api
end

return M
