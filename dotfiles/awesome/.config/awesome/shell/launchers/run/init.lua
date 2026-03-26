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

-- =========================================================================
-- Helpers
-- =========================================================================

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

local function resolve_web_cfg(cfg)
	local apps_cfg = cfg.apps or {}
	local launchers_cfg = cfg.launchers or {}
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
	local cfg = args.cfg or {}
	local web_cfg = args.web_cfg or {}

	return {
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
		apps = cfg.apps or {},
		modkey = cfg.input and cfg.input.modkey or "Mod4",

		hide_menu_popup = function()
			if args.handle and args.handle.close then
				args.handle.close()
			end
		end,
	}
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(args)
	args = args or {}

	M.api = {
		ui = args.ui or {},
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

	local _ui = api().ui or {}
	if Theme and type(Theme.init) == "function" then
		Theme.init({
			ui = _ui,
		})
	end

	local lib = (Lib.api and Lib.api.lib) or {}
	local Button = lib.button
	local Actions = lib.actions

	local cfg = opts.cfg or {}
	local web_cfg = resolve_web_cfg(cfg)

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

	local ui = build_view_ui(panel, search, d)
	local view = View.build(ui, textbox)
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

	local handle = Lib.ui_api.open_panel(stack, panel, {
		use_backdrop = false,
		show_root = false,
		screen = opts.screen or (mouse and mouse.screen) or awful.screen.focused(),
		shape = function(cr, w, h)
			local r = tonumber(panel.panel_radius)
			gears.shape.rounded_rect(cr, w, h, r)
		end,
	})

	local ctrl = Controller.new(build_controller_ctx({
		awful = awful,
		gears = gears,
		prompt = prompt,
		view = view,
		textbox = textbox,
		search = search,
		cfg = cfg,
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
		handle.on_close(cleanup_api)
	end

	if handle and type(handle.close) == "function" then
		local old_close = handle.close
		handle.close = function(...)
			cleanup_api()
			return old_close(...)
		end
	end

	return handle
end

return M
