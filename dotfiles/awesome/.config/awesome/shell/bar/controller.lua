-- ~/.config/awesome/shell/bar/controller.lua
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

local function ui_api(api)
	return (api and api.ui) or {}
end

local function themes_api(api)
	return (api and api.themes) or {}
end

local function bar_api(api)
	return (api and api.bar) or {}
end

local function theme(api, name)
	return themes_api(api)[name]
end

local function bar_mod(api, name)
	return bar_api(api)[name]
end

local function ensure_ui(cfg, api)
	local _ui = ui_api(api)

	if _ui and _ui.theme and _ui.theme.colors then
		return _ui
	end

	local UI = require("ui")
	local ui_mod = UI.init({ cfg = cfg })
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

local function init_themes(api, cfg, _ui)
	local TWibar = theme(api, "wibar")
	local TStart = theme(api, "start")
	local TTabs = theme(api, "tabs")

	if TWibar and type(TWibar.init) == "function" then
		TWibar.init({
			cfg = cfg,
			ui = _ui,
		})
	end

	if TStart and type(TStart.init) == "function" then
		TStart.init({
			cfg = cfg,
			ui = _ui,
		})
	end

	if TTabs and type(TTabs.init) == "function" then
		TTabs.init({
			cfg = cfg,
			ui = _ui,
		})
	end

	local start_theme = (TStart and type(TStart.get) == "function" and TStart.get()) or {}
	local tabs_theme = (TTabs and type(TTabs.get) == "function" and TTabs.get()) or {}

	if type(start_theme) ~= "table" then
		start_theme = {}
	end

	if type(tabs_theme) ~= "table" then
		tabs_theme = {}
	end

	return {
		wibar = TWibar,
		start = start_theme,
		tabs = tabs_theme,
	}
end

local function resolve_props(wibar_theme, bar_position)
	local props = (wibar_theme and type(wibar_theme.props) == "function" and wibar_theme.props()) or {}

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
	local Sections = bar_mod(api, "sections")

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
	local Reveal = bar_mod(api, "reveal")
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
	local setup_args = args.args or {}
	local api_ref = args.api or {}

	local cfg = setup_args.cfg or {}
	local menu_api = setup_args.menu_api

	local input_cfg = cfg.input or {}
	local bar_cfg = cfg.bar or {}

	local modkey = input_cfg.modkey or "Mod4"
	local showtray = (setup_args.systray ~= false)

	local Policy = bar_mod(api_ref, "policy")
	local Widgets = bar_mod(api_ref, "widgets")

	local visibility = resolve_visibility(Policy, s, bar_cfg)
	local start_action = resolve_start_action(Policy, bar_cfg)

	local _ui = ensure_ui(cfg, api_ref)
	local themes = init_themes(api_ref, cfg, _ui)
	local props = resolve_props(themes.wibar, bar_cfg.position)

	local parts = {}

	if Widgets and type(Widgets.init) == "function" then
		Widgets.init({
			api = api_ref,
			cfg = cfg,
			ui = _ui,
		})
	end

	if Widgets and type(Widgets.build) == "function" then
		parts = Widgets.build(s, {
			cfg = cfg,
			modkey = modkey,
			props = props,
			start_theme = themes.start,
			tabs_theme = themes.tabs,
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
