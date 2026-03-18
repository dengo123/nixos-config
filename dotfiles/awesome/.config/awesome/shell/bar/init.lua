-- ~/.config/awesome/shell/bar/init.lua
local awful = require("awful")
local beautiful = require("beautiful")
local gears = require("gears")
local wibox = require("wibox")

local Clock = require("shell.bar.widgets.clock")
local Reveal = require("shell.bar.reveal")
local Start = require("shell.bar.widgets.start")
local Systray = require("shell.bar.widgets.systray")
local Tabs = require("shell.bar.widgets.tabs")
local Tags = require("shell.bar.widgets.tags")
local Notify = require("shell.bar.widgets.notify")

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

local function hspace(px)
	return wibox.widget({
		widget = wibox.widget.separator,
		forced_width = px or 0,
		opacity = 0,
	})
end

local function compute_tabs_leading_gap(show_start, show_tags)
	local gap = 0

	if not show_start then
		gap = gap + 8
	end

	if not show_tags then
		gap = gap + 6
	end

	return gap
end

local function ensure_layoutbox(s)
	if not s.mylayoutbox or not s.mylayoutbox.valid then
		s.mylayoutbox = awful.widget.layoutbox(s)
		s.mylayoutbox:buttons(gears.table.join(
			awful.button({}, 1, function()
				awful.layout.inc(1)
			end),
			awful.button({}, 3, function()
				awful.layout.inc(-1)
			end),
			awful.button({}, 4, function()
				awful.layout.inc(1)
			end),
			awful.button({}, 5, function()
				awful.layout.inc(-1)
			end)
		))
	end

	return wibox.widget({
		s.mylayoutbox,
		left = beautiful.layoutbox_pad_h or 0,
		right = beautiful.layoutbox_pad_h or 0,
		top = beautiful.layoutbox_pad_v or 0,
		bottom = beautiful.layoutbox_pad_v or 0,
		widget = wibox.container.margin,
	})
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.setup(s, args)
	args = args or {}

	-- ---------------------------------------------------------------------
	-- Config
	-- ---------------------------------------------------------------------

	local cfg = args.cfg or {}
	local ui = args.ui
	local menu_api = args.menu_api

	local system_cfg = cfg.system or {}
	local tags_cfg = cfg.tags or {}
	local bar_cfg = cfg.bar or {}
	local clock_cfg = bar_cfg.clock or {}

	local modkey = system_cfg.modkey or "Mod4"
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

	local empty = empty_widget()

	-- ---------------------------------------------------------------------
	-- Theme
	-- ---------------------------------------------------------------------

	local theme = ui and ui.theme or nil
	local wibar_theme = theme and theme.wibar or require("theme.wibar")
	local tabs_theme = (theme and theme.tabs and theme.tabs.get) and theme.tabs.get(cfg.tabs or {}) or nil
	local start_theme = (theme and theme.start and theme.start.get) and theme.start.get(cfg.start or {}) or nil
	local menu_theme = cfg.menus

	if wibar_theme and wibar_theme.init then
		pcall(wibar_theme.init, cfg)
	end

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
		} or nil,
	})

	local tags = show_tags and Tags.build(s, {}) or { indicator = empty }

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

	local layoutbox = ensure_layoutbox(s)

	local start_btn = show_start
			and Start.build({
				screen = s,
				theme = start_theme,
				bar_height = props.height,
				launcher = system_cfg.launcher,
				terminal = system_cfg.terminal,
				menu = cfg.mymainmenu,
				menu_api = menu_api and {
					show_for_widget = function(widget)
						menu_api.show_for_start_widget(s, widget)
					end,
					get_start_items = function()
						return menu_api.get_start_items()
					end,
				} or nil,
			})
		or empty

	local notify = show_notify and Notify.build(s, {}) or empty

	-- ---------------------------------------------------------------------
	-- Sections
	-- ---------------------------------------------------------------------

	local tabs_leading_spacer = hspace(compute_tabs_leading_gap(show_start, show_tags))

	local parts = {
		start_btn = start_btn or empty,
		tags = tags or { indicator = empty },
		tabs = tabs or { tasklist = empty },
		tray = tray or empty,
		notify = notify or empty,
		clock = clock or empty,
		layoutbox = layoutbox or empty,
		tabs_leading_spacer = tabs_leading_spacer,
		spacing_l = 8,
		spacing_r = 0,
	}

	local notify_gap_left = beautiful.notify_button_gap_left
	local notify_zone_width = beautiful.notify_button_zone_width
	local notify_seam_offset = beautiful.notify_button_seam_offset
	local systray_bg = beautiful.systray_bg or beautiful.bg_systray or beautiful.wibar_bg
	local bar_height = beautiful.wibar_height

	local notify_zone = wibox.widget({
		{
			forced_width = show_notify and notify_zone_width or 0,
			forced_height = bar_height,
			bg = show_notify and systray_bg or "#00000000",
			widget = wibox.container.background,
		},
		{
			{
				parts.notify,
				halign = "left",
				valign = "center",
				widget = wibox.container.place,
			},
			left = show_notify and -notify_seam_offset or 0,
			widget = wibox.container.margin,
		},
		layout = wibox.layout.stack,
	})

	local tray_cluster = wibox.widget({
		{
			notify_zone,
			parts.tray,
			layout = wibox.layout.fixed.horizontal,
		},
		bg = systray_bg,
		widget = wibox.container.background,
	})

	local notify_with_gap = wibox.widget({
		tray_cluster,
		left = show_notify and notify_gap_left or 0,
		widget = wibox.container.margin,
	})

	local sections = (wibar_theme.layout and wibar_theme.layout(s, parts))
		or {
			left = {
				layout = wibox.layout.fixed.horizontal,
				parts.start_btn,
				parts.tags.indicator,
				parts.tabs_leading_spacer,
				parts.tabs.tasklist,
			},
			center = nil,
			right = {
				layout = wibox.layout.fixed.horizontal,
				parts.layoutbox,
				notify_with_gap,
				parts.clock,
			},
		}

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
