-- features/shell/menu/search/controller.lua
-- Verantwortlich für: State, Styles umschalten, Root-Click-Watcher, Prompt-Lifecycle.

local M = {}

local function try(fn)
	return function(...)
		local ok = pcall(fn, ...)
		return ok
	end
end

function M.new(ctx)
	-- Erwartet:
	-- ctx.parts = { textbox, prefix_lbl, inner_margin, bg_box, width_ctl }
	-- ctx.sizes = { width_expanded, width_collapsed }
	-- ctx.colors = { bg_active, bg_collapsed, fg_active, cursor_bg, cursor_fg }
	-- ctx.layout = { left, right, top, bottom }
	-- ctx.prefixes = { local_mode, web_mode }
	-- ctx.providers = Providers
	-- ctx.web = { browser, engine }
	-- ctx.home = HOME
	-- ctx.awful, ctx.gears
	-- ctx.hide_menu_popup (fn)

	local awful, gears = ctx.awful, ctx.gears
	local textbox = ctx.parts.textbox
	local prefix_lbl = ctx.parts.prefix_lbl
	local inner_margin = ctx.parts.inner_margin
	local bg_box = ctx.parts.bg_box
	local width_ctl = ctx.parts.width_ctl

	local WIDTH_EXP = ctx.sizes.width_expanded
	local WIDTH_COL = ctx.sizes.width_collapsed

	local BG_ACTIVE = ctx.colors.bg_active
	local BG_COLLAPSED = ctx.colors.bg_collapsed
	local FG_ACTIVE = ctx.colors.fg_active
	local CURSOR_BG = ctx.colors.cursor_bg
	local CURSOR_FG = ctx.colors.cursor_fg

	local PAD_L, PAD_R, PAD_T, PAD_B = ctx.layout.left, ctx.layout.right, ctx.layout.top, ctx.layout.bottom

	local PREFIX_LOCAL = ctx.prefixes.local_mode
	local PREFIX_WEB = ctx.prefixes.web_mode

	local Providers = ctx.providers
	local BROWSER = ctx.web.browser
	local ENGINE_FMT = ctx.web.engine
	local HOME = ctx.home

	local state = {
		prompt_running = false,
		collapsed = true,
		saved_root_buttons = nil,
		ignore_next_root_click = false,
		mode = "local",
	}

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
			-- einklappen
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
		inner_margin.left, inner_margin.right, inner_margin.top, inner_margin.bottom = PAD_L, PAD_R, PAD_T, PAD_B
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
		prefix_lbl.text = (state.mode == "local") and PREFIX_LOCAL or PREFIX_WEB

		gears.timer.delayed_call(function()
			state.prompt_running = true
			-- Sicherheit: Styles auf dem Textfeld
			try(function()
				textbox.bg = BG_ACTIVE
			end)()
			try(function()
				textbox.fg = FG_ACTIVE
			end)()
			try(function()
				textbox.bg_cursor = CURSOR_BG
			end)()
			try(function()
				textbox.fg_cursor = CURSOR_FG
			end)()

			awful.prompt.run({
				prompt = "",
				textbox = textbox,
				history_path = nil,
				completion_callback = nil,

				exe_callback = function(q)
					q = (q or ""):match("^%s*(.-)%s*$")
					if state.mode == "local" then
						Providers.local_search(q, HOME)
					else
						Providers.web_search(q, ENGINE_FMT, BROWSER)
					end
					collapse()
					if ctx.hide_menu_popup then
						ctx.hide_menu_popup()
					end
				end,

				done_callback = function()
					collapse()
				end,

				keypressed_callback = function(mod, key)
					if (not mod or #mod == 0) and key == "Escape" then
						collapse()
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

	local function expand_and_start(mode)
		stop_prompt()
		apply_expanded_style()
		attach_root_click_watcher()
		run_prompt(mode or "local")
	end

	-- Public API
	local api = {
		-- lifecycle
		init = function()
			apply_collapsed_style()
		end,
		bind_mouse = function(widget)
			widget:buttons(gears.table.join(
				awful.button({}, 1, function()
					state.ignore_next_root_click = true
					gears.timer.delayed_call(function()
						state.ignore_next_root_click = false
					end)
					expand_and_start("local")
				end),
				awful.button({}, 3, function()
					state.ignore_next_root_click = true
					gears.timer.delayed_call(function()
						state.ignore_next_root_click = false
					end)
					expand_and_start("web")
				end)
			))
		end,

		-- external controls
		focus_local = function()
			expand_and_start("local")
		end,
		focus_web = function()
			expand_and_start("web")
		end,
		cancel = function()
			collapse()
		end,

		-- state queries
		is_active = function()
			return state.prompt_running
		end,
		is_collapsed = function()
			return state.collapsed
		end,

		-- runtime config
		set_engine = function(s)
			ENGINE_FMT = s or ENGINE_FMT
		end,
		set_browser = function(b)
			BROWSER = b or BROWSER
		end,
	}

	return api
end

return M
