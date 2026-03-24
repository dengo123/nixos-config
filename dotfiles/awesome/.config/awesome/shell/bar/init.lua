-- ~/.config/awesome/shell/bar/init.lua
local awful = require("awful")
local beautiful = require("beautiful")
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

local reveal_signals_ready = false

-- =========================================================================
-- Helpers
-- =========================================================================

local function api()
	return M.api or {}
end

local function widgets_api()
	return api().widgets or {}
end

local function themes_api()
	return api().themes or {}
end

local function bar_api()
	return api().bar or {}
end

local function ui_api()
	return api().ui or {}
end

local function widget(name)
	return widgets_api()[name]
end

local function theme(name)
	return themes_api()[name]
end

local function bar_mod(name)
	return bar_api()[name]
end

local function empty_widget()
	return wibox.widget({
		widget = wibox.widget.separator,
		forced_width = 0,
		opacity = 0,
	})
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(args)
	args = args or {}

	M.api = {
		ui = args.ui or {},
		widgets = {
			clock = safe_require("shell.bar.widgets.clock"),
			layoutbox = safe_require("shell.bar.widgets.layoutbox"),
			notify = safe_require("shell.bar.widgets.notify"),
			start = safe_require("shell.bar.widgets.start"),
			systray = safe_require("shell.bar.widgets.systray"),
			tabs = safe_require("shell.bar.widgets.tabs"),
			tags = safe_require("shell.bar.widgets.tags"),
		},
		themes = {
			start = safe_require("shell.bar.themes.start"),
			tabs = safe_require("shell.bar.themes.tabs"),
			wibar = safe_require("shell.bar.themes.wibar"),
		},
		bar = {
			policy = safe_require("shell.bar.policy"),
			reveal = safe_require("shell.bar.reveal"),
			sections = safe_require("shell.bar.sections"),
		},
	}

	return M
end

function M.setup(s, args)
	args = args or {}

	local cfg = args.cfg or {}
	local menu_api = args.menu_api

	local input_cfg = cfg.input or {}
	local bar_cfg = cfg.bar or {}
	local clock_cfg = bar_cfg.clock or {}

	local modkey = input_cfg.modkey or "Mod4"
	local showtray = (args.systray ~= false)

	local Policy = bar_mod("policy")
	local Reveal = bar_mod("reveal")
	local Sections = bar_mod("sections")

	local WClock = widget("clock")
	local WLayoutbox = widget("layoutbox")
	local WNotify = widget("notify")
	local WStart = widget("start")
	local WSystray = widget("systray")
	local WTabs = widget("tabs")
	local WTags = widget("tags")

	local TStart = theme("start")
	local TTabs = theme("tabs")
	local TWibar = theme("wibar")

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

	local start_action = "menu"
	if Policy and type(Policy.start_action) == "function" then
		start_action = Policy.start_action(bar_cfg)
	end

	local bar_position = bar_cfg.position

	local reveal_trigger_px = tonumber(bar_cfg.reveal_trigger_px) or 2
	local reveal_hide_delay = tonumber(bar_cfg.reveal_hide_delay) or 0.20

	local show_start = start_visible_on_screen
	local show_notify = notify_visible_on_screen
	local show_tags = tags_visible_on_screen

	local _ui = ui_api()

	if not (_ui and _ui.theme and _ui.theme.colors) then
		local UI = require("ui")
		local ui_mod = UI.init({ cfg = cfg })
		_ui = ui_mod.get()
	end

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

	local wibar_theme = TWibar
	local start_theme = (TStart and type(TStart.get) == "function" and TStart.get()) or {}
	local tabs_theme = (TTabs and type(TTabs.get) == "function" and TTabs.get()) or {}

	if type(start_theme) ~= "table" then
		start_theme = {}
	end

	if type(tabs_theme) ~= "table" then
		tabs_theme = {}
	end

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

	local tabs = WTabs
			and WTabs.build
			and WTabs.build(s, {
				modkey = modkey,
				group_by_class = true,
				theme = tabs_theme,
				bar_height = props.height,
				menu_api = menu_api and {
					show_for_widget_with_clients_at = function(widget_, clients, anchor)
						menu_api.show_for_tabs_widget_with_clients_at(s, widget_, clients, anchor)
					end,
					is_open = function()
						return menu_api.is_open()
					end,
					hide = function()
						return menu_api.hide()
					end,
				} or nil,
			})
		or nil

	local tags = show_tags and WTags and WTags.build and WTags.build(s, {
		cfg = cfg,
	}) or nil

	local tray = showtray and WSystray and WSystray.build and WSystray.build({}) or nil

	local clock = WClock
			and WClock.build
			and WClock.build(s, {
				show_seconds = (clock_cfg.show_seconds == true),
				app = cfg.apps.calendar,
				calendar_enable = (clock_cfg.calendar_enable ~= false),
				calendar_use_menu_theme = (clock_cfg.calendar_use_menu_theme == true),
				bar_position = props.position,
			})
		or nil

	local layoutbox = WLayoutbox and WLayoutbox.build and WLayoutbox.build(s) or nil

	local start_btn = show_start
			and WStart
			and WStart.build
			and WStart.build({
				screen = s,
				theme = start_theme,
				bar_height = props.height,
				cfg = cfg,
				start_action = start_action,
				menu_api = menu_api and {
					show_for_start_widget = function(_screen, widget_)
						menu_api.show_for_start_widget(s, widget_)
					end,
					hide = function()
						menu_api.hide()
					end,
					is_open = function()
						return menu_api.is_open()
					end,
					get_start_items = function()
						return menu_api.get_start_items()
					end,
				} or nil,
			})
		or nil

	local notify = show_notify and WNotify and WNotify.build and WNotify.build(s, {}) or nil

	local sections = Sections
			and Sections.build
			and Sections.build({
				show_start = show_start,
				show_tags = show_tags,
				show_notify = show_notify,
				start_btn = start_btn or empty_widget(),
				tags = tags or { indicator = empty_widget() },
				tabs = tabs or { tasklist = empty_widget() },
				tray = tray or empty_widget(),
				notify = notify or empty_widget(),
				clock = clock or empty_widget(),
				layoutbox = layoutbox or empty_widget(),
			})
		or {
			left = empty_widget(),
			center = nil,
			right = empty_widget(),
		}

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

	s.mywibar:setup({
		layout = wibox.layout.align.horizontal,
		sections.left,
		sections.center,
		sections.right,
	})

	if tray and s == (screen.primary or awful.screen.focused()) then
		awesome.emit_signal("ui::tray_ready", s)
	end

	if Reveal and not reveal_signals_ready and type(Reveal.init_signals) == "function" then
		Reveal.init_signals()
		reveal_signals_ready = true
	end

	if Reveal and type(Reveal.attach) == "function" then
		Reveal.attach(s, s.mywibar, {
			edge = props.position,
			trigger_px = reveal_trigger_px,
			hide_delay = reveal_hide_delay,
			layout_peek_duration = tonumber(bar_cfg.layout_peek_duration) or 0.70,
		})
	end

	return s.mywibar
end

return M
