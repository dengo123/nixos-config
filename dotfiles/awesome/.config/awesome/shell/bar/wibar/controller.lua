-- ~/.config/awesome/shell/bar/wibar/controller.lua
local awful = require("awful")
local beautiful = require("beautiful")
local wibox = require("wibox")

local M = {}

local reveal_signals_ready = false

-- =========================================================================
-- Helpers
-- =========================================================================

local function empty_widget()
	return wibox.widget({
		widget = wibox.widget.separator,
		forced_width = 0,
		opacity = 0,
	})
end

local function ctx(args)
	return (args and args.ctx) or {}
end

local function setup_args(args)
	return (args and args.args) or {}
end

local function cfg(args)
	local c = ctx(args)
	if c.cfg then
		return c.cfg
	end
	return setup_args(args).cfg or {}
end

local function root_ui_api(api)
	return (api and api.root_ui) or {}
end

local function bar_ui_api(api)
	return (api and api.bar_ui) or {}
end

local function wibar_api(api)
	return (api and api.wibar) or {}
end

local function bar_ui_mod(api, name)
	return bar_ui_api(api)[name]
end

local function wibar_mod(api, name)
	return wibar_api(api)[name]
end

local function policy_mod(api)
	return api and api.policy or nil
end

local function reveal_mod(api)
	return api and api.reveal or nil
end

local function ensure_ui(conf, api)
	local _ui = root_ui_api(api)

	if _ui and _ui.theme and _ui.theme.colors then
		return _ui
	end

	local UI = require("ui")
	local ui_mod = UI.init({ cfg = conf })
	return ui_mod.get()
end

local function resolve_visibility(Policy, s, bar_cfg)
	local bar_normally_visible = true
	if Policy and type(Policy.bar_enabled_on_screen) == "function" then
		bar_normally_visible = (Policy.bar_enabled_on_screen(s, bar_cfg) == true)
	end

	local tags_visible_on_screen = true
	if Policy and type(Policy.tags_enabled_on_screen) == "function" then
		tags_visible_on_screen = (Policy.tags_enabled_on_screen(s, bar_cfg) == true)
	end

	local start_visible_on_screen = true
	if Policy and type(Policy.start_enabled_on_screen) == "function" then
		start_visible_on_screen = (Policy.start_enabled_on_screen(s, bar_cfg) == true)
	end

	local notify_visible_on_screen = true
	if Policy and type(Policy.notify_enabled_on_screen) == "function" then
		notify_visible_on_screen = (Policy.notify_enabled_on_screen(s, bar_cfg) == true)
	end

	return {
		bar = bar_normally_visible,
		tags = tags_visible_on_screen,
		start = start_visible_on_screen,
		notify = notify_visible_on_screen,
	}
end

local function resolve_start_action(Policy, bar_cfg)
	local start_action = "menu"

	if Policy and type(Policy.start_action) == "function" then
		start_action = Policy.start_action(bar_cfg)
	end

	return start_action
end

local function init_bar_ui(api, conf, _ui)
	local WibarUI = bar_ui_mod(api, "wibar")
	local StartUI = bar_ui_mod(api, "start")
	local TabsUI = bar_ui_mod(api, "tabs")

	if WibarUI and type(WibarUI.init) == "function" then
		WibarUI.init({
			cfg = conf,
			ui = _ui,
		})
	end

	if StartUI and type(StartUI.init) == "function" then
		StartUI.init({
			cfg = conf,
			ui = _ui,
		})
	end

	if TabsUI and type(TabsUI.init) == "function" then
		TabsUI.init({
			cfg = conf,
			ui = _ui,
		})
	end

	local start_theme = (StartUI and type(StartUI.get) == "function" and StartUI.get()) or {}
	local tabs_theme = (TabsUI and type(TabsUI.get) == "function" and TabsUI.get()) or {}

	if type(start_theme) ~= "table" then
		start_theme = {}
	end

	if type(tabs_theme) ~= "table" then
		tabs_theme = {}
	end

	return {
		wibar = WibarUI,
		start = start_theme,
		tabs = tabs_theme,
	}
end

local function resolve_props(wibar_ui, bar_position)
	local props = (wibar_ui and type(wibar_ui.props) == "function" and wibar_ui.props()) or {}

	if type(props) ~= "table" then
		props = {}
	end

	props.height = tonumber(props.height) or tonumber(beautiful.wibar_height) or 28
	props.bg = props.bg or beautiful.wibar_bg or beautiful.bg_normal
	props.fg = props.fg or beautiful.wibar_fg or beautiful.fg_normal
	props.on_top = (props.on_top ~= nil) and props.on_top or beautiful.wibar_on_top
	props.opacity = tonumber(props.opacity) or tonumber(beautiful.wibar_opacity) or 1
	props.shape = props.shape or beautiful.wibar_shape
	props.margins = props.margins or beautiful.wibar_margins
	props.position = bar_position or "bottom"

	return props
end

local function build_sections(api, parts, visibility)
	local Sections = wibar_mod(api, "sections")

	if Sections and type(Sections.build) == "function" then
		return Sections.build({
			show_start = visibility.start,
			show_tags = visibility.tags,
			show_notify = visibility.notify,
			start_btn = parts.start_btn or empty_widget(),
			tags = parts.tags or { indicator = empty_widget() },
			tabs = parts.tabs or { tasklist = empty_widget() },
			tray = parts.tray or empty_widget(),
			notify = parts.notify or empty_widget(),
			clock = parts.clock or empty_widget(),
			layoutbox = parts.layoutbox or empty_widget(),
		})
	end

	return {
		left = empty_widget(),
		center = nil,
		right = empty_widget(),
	}
end

local function create_wibar(s, props, sections, bar_normally_visible)
	local old = s.mywibar

	if old and old.valid then
		old.visible = false
		pcall(function()
			old:struts(nil)
		end)
		pcall(function()
			old.widget = nil
		end)
		s.mywibar = nil
	end

	s.mywibar = awful.wibar({
		position = props.position,
		screen = s,
		height = props.height,
		bg = props.bg,
		fg = props.fg,
		ontop = props.on_top,
		opacity = props.opacity,
		shape = props.shape,
		margins = props.margins,
		visible = bar_normally_visible,
	})

	s.mywibar._normal_visibility_enabled = bar_normally_visible
	s.mywibar._is_fixed_bar = (bar_normally_visible == true)
	s.mywibar._menu_anchor = (bar_normally_visible == true)

	s.mywibar:setup({
		layout = wibox.layout.align.horizontal,
		sections.left,
		sections.center,
		sections.right,
	})

	return s.mywibar
end

local function attach_reveal(api, s, bar, bar_cfg, props)
	local Reveal = reveal_mod(api)
	if not Reveal then
		return
	end

	if not reveal_signals_ready and type(Reveal.init_signals) == "function" then
		Reveal.init_signals()
		reveal_signals_ready = true
	end

	if type(Reveal.attach) == "function" then
		Reveal.attach(s, bar, {
			edge = props.position,
			trigger_px = tonumber(bar_cfg.reveal_trigger_px) or 2,
			hide_delay = tonumber(bar_cfg.reveal_hide_delay) or 0.20,
			layout_peek_duration = tonumber(bar_cfg.layout_peek_duration) or 0.70,
		})
	end
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.setup(args)
	args = args or {}

	local s = args.screen
	if not s then
		return nil
	end

	local api_ref = args.api or {}
	local conf = cfg(args)
	local legacy_args = setup_args(args)
	local c = ctx(args)

	local menu_api = legacy_args.menu_api
		or (c.features and c.features.menu)
		or (c.shell and c.shell.menu)
		or (c.api and c.api.menu)

	local input_cfg = conf.input or {}
	local bar_cfg = conf.bar or {}

	local modkey = c.modkey or input_cfg.modkey or "Mod4"
	local showtray = (legacy_args.systray ~= false)

	local Policy = policy_mod(api_ref)
	local Widgets = wibar_mod(api_ref, "widgets")

	local visibility = resolve_visibility(Policy, s, bar_cfg)
	local start_action = resolve_start_action(Policy, bar_cfg)

	local _ui = ensure_ui(conf, api_ref)
	local bar_ui = init_bar_ui(api_ref, conf, _ui)
	local props = resolve_props(bar_ui.wibar, bar_cfg.position)

	local parts = {}

	if Widgets and type(Widgets.init) == "function" then
		Widgets.init({
			ctx = c,
			api = api_ref,
			cfg = conf,
			ui = _ui,
		})
	end

	if Widgets and type(Widgets.build) == "function" then
		parts = Widgets.build(s, {
			ctx = c,
			cfg = conf,
			modkey = modkey,
			props = props,
			start_theme = bar_ui.start,
			tabs_theme = bar_ui.tabs,
			showtray = showtray,
			show_start = visibility.start,
			show_tags = visibility.tags,
			show_notify = visibility.notify,
			start_action = start_action,
			menu_api = menu_api,
		}) or {}
	end

	local sections = build_sections(api_ref, parts, visibility)
	local bar = create_wibar(s, props, sections, visibility.bar)

	if parts.tray and s == (screen.primary or awful.screen.focused()) then
		awesome.emit_signal("ui::tray_ready", s)
	end

	attach_reveal(api_ref, s, bar, bar_cfg, props)

	return bar
end

return M
