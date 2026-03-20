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

local Widgets = {
	clock = safe_require("shell.bar.widgets.clock"),
	layoutbox = safe_require("shell.bar.widgets.layoutbox"),
	notify = safe_require("shell.bar.widgets.notify"),
	start = safe_require("shell.bar.widgets.start"),
	systray = safe_require("shell.bar.widgets.systray"),
	tabs = safe_require("shell.bar.widgets.tabs"),
	tags = safe_require("shell.bar.widgets.tags"),
}

local Themes = {
	start = safe_require("shell.bar.themes.start"),
	tabs = safe_require("shell.bar.themes.tabs"),
	wibar = safe_require("shell.bar.themes.wibar"),
}

local Bar = {
	policy = safe_require("shell.bar.policy"),
	reveal = safe_require("shell.bar.reveal"),
	sections = safe_require("shell.bar.sections"),
}

assert(Widgets.clock and type(Widgets.clock) == "table", "bar.init: widgets.clock fehlt")
assert(Widgets.layoutbox and type(Widgets.layoutbox) == "table", "bar.init: widgets.layoutbox fehlt")
assert(Widgets.notify and type(Widgets.notify) == "table", "bar.init: widgets.notify fehlt")
assert(Widgets.start and type(Widgets.start) == "table", "bar.init: widgets.start fehlt")
assert(Widgets.systray and type(Widgets.systray) == "table", "bar.init: widgets.systray fehlt")
assert(Widgets.tabs and type(Widgets.tabs) == "table", "bar.init: widgets.tabs fehlt")
assert(Widgets.tags and type(Widgets.tags) == "table", "bar.init: widgets.tags fehlt")

assert(Themes.start and type(Themes.start) == "table", "bar.init: themes.start fehlt")
assert(Themes.tabs and type(Themes.tabs) == "table", "bar.init: themes.tabs fehlt")
assert(Themes.wibar and type(Themes.wibar) == "table", "bar.init: themes.wibar fehlt")

assert(Bar.policy and type(Bar.policy) == "table", "bar.init: policy fehlt")
assert(Bar.reveal and type(Bar.reveal) == "table", "bar.init: reveal fehlt")
assert(Bar.sections and type(Bar.sections) == "table", "bar.init: sections fehlt")

local M = {}

local reveal_signals_ready = false

-- =========================================================================
-- Public API
-- =========================================================================

function M.setup(s, args)
	args = args or {}

	-- ---------------------------------------------------------------------
	-- Config
	-- ---------------------------------------------------------------------

	local cfg = args.cfg or {}
	local menu_api = args.menu_api

	local input_cfg = cfg.input or {}
	local tags_cfg = cfg.tags or {}
	local bar_cfg = cfg.bar or {}
	local clock_cfg = bar_cfg.clock or {}

	local modkey = input_cfg.modkey or "Mod4"
	local showtray = (args.systray ~= false)

	local bar_normally_visible = Bar.policy.bar_enabled_on_screen(s, bar_cfg)
	local start_enabled = Bar.policy.start_enabled_on_screen(s, bar_cfg)
	local bar_position = bar_cfg.position
	local bar_notify_mode = tostring(bar_cfg.show_notify or "primary"):lower()

	local reveal_trigger_px = tonumber(bar_cfg.reveal_trigger_px) or 2
	local reveal_hide_delay = tonumber(bar_cfg.reveal_hide_delay) or 0.20

	local selection_mode = tostring(tags_cfg.selection or "single"):lower()
	local tags_on_primary_only = (selection_mode == "sync")

	local primary = screen.primary or awful.screen.focused()
	local is_primary = (s == primary)

	local show_start = start_enabled
	local show_tags = bar_normally_visible and ((not tags_on_primary_only) or is_primary)
	local show_notify = bar_normally_visible and ((bar_notify_mode ~= "primary") or is_primary)

	-- ---------------------------------------------------------------------
	-- Theme
	-- ---------------------------------------------------------------------

	pcall(Themes.wibar.init, cfg)
	pcall(Themes.start.init, cfg)
	pcall(Themes.tabs.init, cfg)

	local wibar_theme = Themes.wibar
	local start_theme = Themes.start.get()
	local tabs_theme = Themes.tabs.get()
	local menu_theme = cfg.menus

	-- ---------------------------------------------------------------------
	-- Props
	-- ---------------------------------------------------------------------

	local props = (wibar_theme.props and wibar_theme.props())
		or {
			height = beautiful.wibar_height,
			bg = beautiful.wibar_bg,
			fg = beautiful.wibar_fg,
			on_top = beautiful.wibar_on_top,
			opacity = beautiful.wibar_opacity,
			shape = beautiful.wibar_shape,
			margins = beautiful.wibar_margins,
		}

	props.position = bar_position or "bottom"

	-- ---------------------------------------------------------------------
	-- Widgets
	-- ---------------------------------------------------------------------

	local tabs = Widgets.tabs.build(s, {
		modkey = modkey,
		group_by_class = true,
		theme = tabs_theme,
		bar_height = props.height,
		menu_theme = menu_theme,
		menu_api = menu_api and {
			show_for_widget_with_clients_at = function(widget, clients, anchor)
				menu_api.show_for_tabs_widget_with_clients_at(s, widget, clients, anchor)
			end,
			is_open = function()
				return menu_api.is_open()
			end,
			hide = function()
				return menu_api.hide()
			end,
		} or nil,
	})

	local tags = show_tags and Widgets.tags.build(s, {}) or nil

	local tray = showtray and Widgets.systray.build({
		menu_theme = menu_theme,
	}) or nil

	local clock = Widgets.clock.build(s, {
		show_seconds = (clock_cfg.show_seconds == true),
		app = cfg.apps.calendar,
		calendar_enable = (clock_cfg.calendar_enable ~= false),
		calendar_use_menu_theme = (clock_cfg.calendar_use_menu_theme == true),
		bar_position = props.position,
	})

	local layoutbox = Widgets.layoutbox.build(s)

	local start_btn = show_start
			and Widgets.start.build({
				screen = s,
				theme = start_theme,
				bar_height = props.height,
				cfg = cfg,
				start_action = Bar.policy.start_action(bar_cfg),
				menu_api = menu_api and {
					show_for_start_widget = function(_screen, widget)
						menu_api.show_for_start_widget(s, widget)
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

	local notify = show_notify and Widgets.notify.build(s, {}) or nil

	-- ---------------------------------------------------------------------
	-- Sections
	-- ---------------------------------------------------------------------

	local empty = wibox.widget({
		widget = wibox.widget.separator,
		forced_width = 0,
		opacity = 0,
	})

	local sections = Bar.sections.build({
		show_start = show_start,
		show_tags = show_tags,
		show_notify = show_notify,
		start_btn = start_btn or empty,
		tags = tags or { indicator = empty },
		tabs = tabs or { tasklist = empty },
		tray = tray or empty,
		notify = notify or empty,
		clock = clock or empty,
		layoutbox = layoutbox or empty,
	})

	-- ---------------------------------------------------------------------
	-- Wibar
	-- ---------------------------------------------------------------------

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

	if not reveal_signals_ready then
		Bar.reveal.init_signals()
		reveal_signals_ready = true
	end

	Bar.reveal.attach(s, s.mywibar, {
		edge = props.position,
		trigger_px = reveal_trigger_px,
		hide_delay = reveal_hide_delay,
	})

	return s.mywibar
end

return M
