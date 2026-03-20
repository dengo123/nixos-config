-- ~/.config/awesome/shell/bar/init.lua
local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")

local Reveal = require("shell.bar.reveal")
local Sections = require("shell.bar.sections")
local Clock = require("shell.bar.widgets.clock")
local Layoutbox = require("shell.bar.widgets.layoutbox")
local Notify = require("shell.bar.widgets.notify")
local Start = require("shell.bar.widgets.start")
local Systray = require("shell.bar.widgets.systray")
local Tabs = require("shell.bar.widgets.tabs")
local Tags = require("shell.bar.widgets.tags")

local StartTheme = require("shell.bar.themes.start")
local TabsTheme = require("shell.bar.themes.tabs")
local WibarTheme = require("shell.bar.themes.wibar")

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

	local start_on_primary_only = (bar_cfg.start_on_primary_only == true)
	local bar_position = bar_cfg.position
	local bar_notify_mode = tostring(bar_cfg.show_notify or "primary"):lower()

	local reveal_on_fullscreen_edge = (bar_cfg.reveal_on_fullscreen_edge == true)
	local reveal_trigger_px = tonumber(bar_cfg.reveal_trigger_px) or 2
	local reveal_hide_delay = tonumber(bar_cfg.reveal_hide_delay) or 0.20

	local selection_mode = tostring(tags_cfg.selection or "single"):lower()
	local tags_on_primary_only = (selection_mode == "sync")

	local primary = screen.primary or awful.screen.focused()
	local is_primary = (s == primary)

	local show_start = (not start_on_primary_only) or is_primary
	local show_tags = (not tags_on_primary_only) or is_primary
	local show_notify = (bar_notify_mode ~= "primary") or is_primary

	-- ---------------------------------------------------------------------
	-- Theme
	-- ---------------------------------------------------------------------

	pcall(WibarTheme.init, cfg)
	pcall(StartTheme.init, cfg)
	pcall(TabsTheme.init, cfg)

	local wibar_theme = WibarTheme
	local start_theme = StartTheme.get()
	local tabs_theme = TabsTheme.get()
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

	local tabs = Tabs.build(s, {
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
				menu_api.hide()
			end,
		} or nil,
	})

	local tags = show_tags and Tags.build(s, {}) or nil

	local tray = showtray and Systray.build({
		menu_theme = menu_theme,
	}) or nil

	local clock = Clock.build(s, {
		show_seconds = (clock_cfg.show_seconds == true),
		app = cfg.apps.calendar,
		calendar_enable = (clock_cfg.calendar_enable ~= false),
		calendar_use_menu_theme = (clock_cfg.calendar_use_menu_theme == true),
		bar_position = props.position,
	})

	local layoutbox = Layoutbox.build(s)

	local start_btn = show_start
			and Start.build({
				screen = s,
				theme = start_theme,
				bar_height = props.height,
				cfg = cfg,
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

	local notify = show_notify and Notify.build(s, {}) or nil

	-- ---------------------------------------------------------------------
	-- Sections
	-- ---------------------------------------------------------------------

	local empty = wibox.widget({
		widget = wibox.widget.separator,
		forced_width = 0,
		opacity = 0,
	})

	local sections = Sections.build({
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
	})

	s.mywibar:setup({
		layout = wibox.layout.align.horizontal,
		sections.left,
		sections.center,
		sections.right,
	})

	if tray and s == (screen.primary or awful.screen.focused()) then
		awesome.emit_signal("ui::tray_ready", s)
	end

	if reveal_on_fullscreen_edge then
		if not reveal_signals_ready then
			Reveal.init_signals()
			reveal_signals_ready = true
		end

		Reveal.attach(s, s.mywibar, {
			edge = props.position,
			trigger_px = reveal_trigger_px,
			hide_delay = reveal_hide_delay,
		})
	end

	return s.mywibar
end

return M
