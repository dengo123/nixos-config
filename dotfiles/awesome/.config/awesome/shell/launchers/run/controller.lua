-- ~/.config/awesome/shell/launchers/run/controller.lua
local M = {}

function M.new(ctx)
	ctx = ctx or {}

	-- =========================================================================
	-- Config
	-- =========================================================================

	local awful = ctx.awful
	local gears = ctx.gears
	local Providers = ctx.providers or {}
	local Complete = ctx.complete or nil

	local parts = ctx.parts or {}
	local view = ctx.view or {}

	local textbox = parts.textbox
	assert(textbox, "run.controller: ctx.parts.textbox fehlt")

	local sizes = ctx.sizes or {}
	local colors = ctx.colors or {}
	local layout = ctx.layout or {}
	local prefixes = ctx.prefixes or {}

	local width_expanded = tonumber(sizes.width_expanded) or 320

	local bg_active = colors.bg_active
	local fg_active = colors.fg_active
	local cursor_bg = colors.cursor_bg
	local cursor_fg = colors.cursor_fg

	local pad_l = tonumber(layout.left) or 0
	local pad_r = tonumber(layout.right) or 0
	local pad_t = tonumber(layout.top) or 0
	local pad_b = tonumber(layout.bottom) or 0

	local prefix_run = prefixes.run_mode or "Run:"
	local prefix_local = prefixes.local_mode or "Files:"
	local prefix_web = prefixes.web_mode or "Browse:"

	local home = ctx.home
	local browser = ctx.web and ctx.web.browser
	local engine = ctx.web and ctx.web.engine
	local apps = ctx.apps or {}

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

	local function complete_ready()
		return Complete
			and type(Complete.ensure) == "function"
			and type(Complete.candidates) == "function"
			and type(Complete.best) == "function"
	end

	local function current_text()
		local text = ""

		if textbox and type(textbox.get_text) == "function" then
			local ok, value = pcall(function()
				return textbox:get_text()
			end)
			if ok and type(value) == "string" then
				text = value
			end
		elseif textbox and type(textbox.text) == "string" then
			text = textbox.text
		end

		return text:match("^%s*(.-)%s*$")
	end

	local function set_text(s)
		s = s or ""

		if textbox and type(textbox.set_text) == "function" then
			pcall(function()
				textbox:set_text(s)
			end)
		elseif textbox then
			textbox.text = s
		end

		if textbox and textbox.cursor then
			textbox.cursor = #s + 1
		end
	end

	local function apply_active_style()
		if type(view.apply_active_style) == "function" then
			view.apply_active_style({
				bg_active = bg_active,
				fg_active = fg_active,
				left = pad_l,
				right = pad_r,
				top = pad_t,
				bottom = pad_b,
				width_expanded = width_expanded,
			})
		end

		try(function()
			textbox.bg = bg_active
		end)

		try(function()
			textbox.fg = fg_active
		end)
	end

	local function set_prefix()
		if type(view.set_prefix) ~= "function" then
			return
		end

		if state.mode == "local" then
			view.set_prefix(prefix_local)
		elseif state.mode == "web" then
			view.set_prefix(prefix_web)
		else
			view.set_prefix(prefix_run)
		end
	end

	local function set_mode(new_mode)
		state.mode = (new_mode == "files") and "local" or (new_mode or "run")
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
		if not complete_ready() then
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
				Providers.local_open(q, home, {
					apps = apps,
				})
			elseif Providers.local_search then
				Providers.local_search(q, home, {
					apps = apps,
				})
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

		if cur ~= "" and complete_ready() then
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
			bg_cursor = cursor_bg,
			fg_cursor = cursor_fg,

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
