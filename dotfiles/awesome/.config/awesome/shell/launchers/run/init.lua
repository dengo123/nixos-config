-- ~/.config/awesome/shell/launchers/run/init.lua
local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")

local function safe_require(path)
	local ok, mod = pcall(require, path)
	if ok then
		return mod
	end

	return nil
end

local M = {
	api = {},
}

local runtime = {
	ctx = {},
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function ctx()
	return runtime.ctx or {}
end

local function cfg()
	return ctx().cfg or {}
end

local function ui()
	return ctx().ui or {}
end

local function api()
	return M.api or {}
end

local function mod(name)
	return api()[name]
end

local function button_label(spec, fallback)
	if type(spec) == "table" then
		return spec.label or fallback
	end

	if type(spec) == "string" and spec ~= "" then
		return spec
	end

	return fallback
end

local function button_id(spec, fallback)
	if type(spec) == "table" then
		return spec.id or fallback
	end

	return fallback
end

local function dims(panel)
	local h = tonumber(panel.height)
	local header_h = tonumber(panel.header_h)
	local footer_h = tonumber(panel.footer_h)
	local pad_h = tonumber(panel.pad_h)
	local pad_v = tonumber(panel.pad_v)

	return {
		w = tonumber(panel.width),
		h = h,
		header_h = header_h,
		footer_h = footer_h,
		body_h = math.max(0, h - header_h - footer_h),
		pad_h = pad_h,
		pad_v = pad_v,
	}
end

local function resolve_theme(overrides)
	local Theme = mod("theme")
	return (Theme and Theme.get and Theme.get(overrides or {})) or {}
end

local function resolve_web_cfg(conf)
	local apps_cfg = conf.apps or {}
	local launchers_cfg = conf.launchers or {}
	local run_cfg = launchers_cfg.run or {}

	return {
		browser = apps_cfg.browser or "firefox",
		engine = run_cfg.web_engine or "https://duckduckgo.com/?q=%s",
	}
end

local function build_view_ui(panel, search, d)
	return {
		body_width = panel.width - 2 * d.pad_h,
		height = tonumber(search.sizes.height),

		bg_active = search.colors.bg_active,
		fg_active = search.colors.fg_active,

		padding = {
			left = search.layout.left,
			right = search.layout.right,
			top = search.layout.top,
			bottom = search.layout.bottom,
		},

		spacing = search.layout.spacing,
		border_w = search.border_w,
		border_color = search.border_color,

		prefix_width = search.prefix_width,
		prefix_font = search.prefix_font,
		prefix_size = search.prefix_size,
		hint = search.hint,
	}
end

local function build_body_widget(view, search, d)
	local bar_h = tonumber(search.sizes.height)
	local hint = search.hint or {}
	local hint_enabled = (hint.show ~= false) and (hint.text and #tostring(hint.text) > 0)

	local pad_t = tonumber(search.layout.top)
	local hint_spacing = tonumber(hint.spacing)
	local hint_size = tonumber(hint.size)

	local hint_h = hint_enabled and (pad_t + hint_size + hint_spacing) or 0
	local offset_in_body = math.max(0, math.floor(d.body_h / 2 - (hint_h + bar_h / 2)))

	return wibox.widget({
		view.widget,
		top = offset_in_body,
		widget = wibox.container.margin,
	})
end

local function build_controller_ctx(args)
	local search = args.search
	local conf = args.cfg or {}
	local web_cfg = args.web_cfg or {}
	local c = args.ctx or ctx()

	return {
		ctx = c,

		awful = args.awful,
		gears = args.gears,
		prompt = args.prompt,
		view = args.view,

		parts = {
			textbox = args.textbox,
		},

		sizes = {
			width_expanded = search.sizes.width_expanded,
		},

		colors = {
			bg_active = search.colors.bg_active,
			fg_active = search.colors.fg_active,
			cursor_bg = search.colors.cursor_bg,
			cursor_fg = search.colors.cursor_fg,
		},

		layout = {
			left = search.layout.left,
			right = search.layout.right,
			top = search.layout.top,
			bottom = search.layout.bottom,
		},

		prefixes = search.prefix,
		providers = args.providers,
		complete = args.complete,
		web = web_cfg,
		home = os.getenv("HOME"),
		apps = conf.apps or {},
		modkey = c.modkey or (conf.input and conf.input.modkey) or "Mod4",

		hide_menu_popup = function()
			if args.handle and args.handle.close then
				args.handle.close()
			end
		end,
	}
end

local function resolve_lib(Lib)
	return (Lib and Lib.lib) or {}
end

local function resolve_open_panel(Lib)
	if Lib and Lib.ui and type(Lib.ui.open_panel) == "function" then
		return Lib.ui.open_panel
	end

	local c = ctx()
	local launchers = (c.shell and c.shell.launchers) or nil

	if launchers and launchers.ui and type(launchers.ui.open_panel) == "function" then
		return launchers.ui.open_panel
	end

	return nil
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(args)
	runtime.ctx = (args and (args.ctx or args)) or {}

	M.api = {
		ui = args and args.ui or ui(),
		container = safe_require("shell.launchers.run.container"),
		view = safe_require("shell.launchers.run.view"),
		providers = safe_require("shell.launchers.run.providers"),
		complete = safe_require("shell.launchers.run.complete"),
		controller = safe_require("shell.launchers.run.controller"),
		theme = safe_require("shell.launchers.run.theme"),
	}

	return M
end

function M.open(opts, Lib)
	opts = opts or {}
	Lib = Lib or {}

	local Container = mod("container")
	local View = mod("view")
	local Providers = mod("providers")
	local Complete = mod("complete")
	local Controller = mod("controller")
	local Theme = mod("theme")

	local conf = opts.cfg or cfg()
	local c = opts.ctx or ctx()
	local _ui = api().ui or ui()

	if Theme and type(Theme.init) == "function" then
		Theme.init({
			ctx = c,
			cfg = conf,
			ui = _ui,
		})
	end

	local lib = resolve_lib(Lib)
	local Button = lib.button
	local Actions = lib.actions
	local open_panel = resolve_open_panel(Lib)

	local web_cfg = resolve_web_cfg(conf)

	local th = resolve_theme(opts.theme)
	local panel = th.panel
	local search = th.search
	local buttons = th.buttons
	local d = dims(panel)

	local mode_label = button_label(buttons.mode, "Mode")
	local ok_label = button_label(buttons.ok, "OK")
	local cancel_label = button_label(buttons.cancel, "Cancel")

	local mode_id = button_id(buttons.mode, "mode")
	local ok_id = button_id(buttons.ok, "ok")
	local cancel_id = button_id(buttons.cancel, "cancel")

	local textbox = wibox.widget.textbox()

	local view = View.build(build_view_ui(panel, search, d), textbox)
	local body_widget = build_body_widget(view, search, d)

	local act_mode = function() end
	local act_ok = function() end
	local act_cancel = function() end

	local mode_btn = Button.mk_button(mode_label, function()
		act_mode()
	end)

	local ok_btn = Button.mk_button(ok_label, function()
		act_ok()
	end)

	local cancel_btn = Button.mk_button(cancel_label, function()
		act_cancel()
	end)

	local stack = Container.build(panel, d, {
		title = opts.title or panel.title,
		body = body_widget,
		footer_buttons = {
			mode_btn,
			ok_btn,
			cancel_btn,
		},
	})

	assert(open_panel, "launchers.run.init: open_panel fehlt")

	local handle = open_panel(stack, panel, {
		use_backdrop = false,
		show_root = false,
		screen = opts.screen or (mouse and mouse.screen) or awful.screen.focused(),
		shape = function(cr, w, h)
			local r = tonumber(panel.panel_radius)
			gears.shape.rounded_rect(cr, w, h, r)
		end,
	})

	local ctrl = Controller.new(build_controller_ctx({
		ctx = c,
		awful = awful,
		gears = gears,
		prompt = prompt,
		view = view,
		textbox = textbox,
		search = search,
		cfg = conf,
		web_cfg = web_cfg,
		providers = Providers,
		complete = Complete,
		handle = handle,
	}))

	ctrl.init()

	rawset(_G, "__run_api", ctrl)

	local start_mode = (opts.mode == "local" or opts.mode == "files") and "local"
		or (opts.mode == "web" and "web")
		or "run"

	if start_mode == "local" then
		ctrl.focus_local()
	elseif start_mode == "web" then
		ctrl.focus_web()
	else
		ctrl.focus_run()
	end

	local bound = Actions.bind({
		ctrl = ctrl,
		handle = handle,
		gears = gears,
	})

	act_mode = bound[mode_id] or act_mode
	act_ok = bound[ok_id] or act_ok
	act_cancel = bound[cancel_id] or act_cancel

	local function cleanup_api()
		if rawget(_G, "__run_api") == ctrl then
			rawset(_G, "__run_api", nil)
		end
	end

	if handle and type(handle.on_close) == "function" then
		handle.on_close(function()
			if ctrl and type(ctrl.cancel) == "function" then
				pcall(function()
					ctrl.cancel()
				end)
			end
			cleanup_api()
		end)
	end

	if handle and type(handle.close) == "function" then
		local old_close = handle.close
		handle.close = function(...)
			if ctrl and type(ctrl.cancel) == "function" then
				pcall(function()
					ctrl.cancel()
				end)
			end
			cleanup_api()
			return old_close(...)
		end
	end

	return handle
end

return M
