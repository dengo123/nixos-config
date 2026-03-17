-- ~/.config/awesome/shell/bar/widgets/clock.lua
local awful = require("awful")
local beautiful = require("beautiful")
local gears = require("gears")
local wibox = require("wibox")

local M = {}

-- =========================================================================
-- Helpers
-- =========================================================================

local function require_number(value, name)
	local n = tonumber(value)
	assert(n ~= nil, "clock: " .. name .. " fehlt/ungültig")
	return n
end

local function require_string(value, name)
	assert(type(value) == "string" and value ~= "", "clock: " .. name .. " fehlt/ungültig")
	return value
end

local function resolve_calendar_attach_pos(bar_position)
	if bar_position == "top" then
		return "tr"
	end

	if bar_position == "right" then
		return "tl"
	end

	if bar_position == "left" then
		return "tr"
	end

	return "br"
end

local function resolve_clock_settings(show_seconds)
	if show_seconds == true then
		return "%H:%M:%S", 1
	end

	return "%H:%M", 60
end

local function normalize_app(app)
	if type(app) ~= "string" then
		return nil
	end

	app = app:match("^%s*(.-)%s*$")

	if app == "" then
		return nil
	end

	return app
end

local function build_calendar(s, clock, opts)
	local use_menu_theme = (opts.calendar_use_menu_theme == true)

	local bg_normal = use_menu_theme and require_string(beautiful.menu_bg_normal, "beautiful.menu_bg_normal")
		or require_string(beautiful.clock_calendar_bg, "beautiful.clock_calendar_bg")

	local fg_normal = use_menu_theme and require_string(beautiful.menu_fg_normal, "beautiful.menu_fg_normal")
		or require_string(beautiful.clock_calendar_fg, "beautiful.clock_calendar_fg")

	local bg_focus = use_menu_theme and require_string(beautiful.menu_bg_focus, "beautiful.menu_bg_focus")
		or require_string(beautiful.clock_calendar_focus, "beautiful.clock_calendar_focus")

	local fg_focus = use_menu_theme and require_string(beautiful.menu_fg_focus, "beautiful.menu_fg_focus")
		or require_string(beautiful.clock_calendar_fg, "beautiful.clock_calendar_fg")

	local border_color = use_menu_theme and require_string(beautiful.menu_border_color, "beautiful.menu_border_color")
		or require_string(beautiful.clock_calendar_border_color, "beautiful.clock_calendar_border_color")

	local border_width = require_number(beautiful.clock_calendar_border_width, "beautiful.clock_calendar_border_width")

	local cal = awful.widget.calendar_popup.month({
		screen = s,
		spacing = 4,
		start_sunday = false,
		long_weekdays = true,
		week_numbers = false,

		style_month = {
			bg_color = bg_normal,
			fg_color = fg_normal,
			padding = 8,
			border_width = border_width,
			border_color = border_color,
		},
		style_header = {
			bg_color = bg_normal,
			fg_color = fg_normal,
			border_width = 0,
		},
		style_weekday = {
			bg_color = bg_normal,
			fg_color = fg_normal,
			border_width = 0,
		},
		style_normal = {
			bg_color = bg_normal,
			fg_color = fg_normal,
			border_width = 0,
			shape_border_width = 0,
		},
		style_focus = {
			bg_color = bg_focus,
			fg_color = fg_focus,
			border_width = 0,
			shape_border_width = 0,
			markup = function(t)
				return "<b>" .. t .. "</b>"
			end,
		},
		style_weeknumbers = {
			bg_color = bg_normal,
			fg_color = fg_normal,
			border_width = 0,
		},
	})

	cal:attach(clock, resolve_calendar_attach_pos(opts.bar_position), {
		on_hover = true,
	})

	return cal
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.build(s, opts)
	opts = opts or {}

	-- ---------------------------------------------------------------------
	-- Theme
	-- ---------------------------------------------------------------------

	local bar_height = require_number(beautiful.wibar_height, "beautiful.wibar_height")
	local pad_h = require_number(beautiful.clock_pad_h, "beautiful.clock_pad_h")
	local pad_v = require_number(beautiful.clock_pad_v, "beautiful.clock_pad_v")
	local bg = require_string(beautiful.clock_bg, "beautiful.clock_bg")
	local fg = require_string(beautiful.clock_fg, "beautiful.clock_fg")

	-- ---------------------------------------------------------------------
	-- Config
	-- ---------------------------------------------------------------------

	local show_seconds = (opts.show_seconds == true)
	local app = normalize_app(opts.app)
	local calendar_enable = (opts.calendar_enable ~= false)
	local calendar_use_menu_theme = (opts.calendar_use_menu_theme == true)
	local bar_position = opts.bar_position or "bottom"

	local format, refresh = resolve_clock_settings(show_seconds)

	-- ---------------------------------------------------------------------
	-- Clock
	-- ---------------------------------------------------------------------

	local clock = wibox.widget.textclock(format, refresh)

	-- ---------------------------------------------------------------------
	-- Calendar
	-- ---------------------------------------------------------------------

	if calendar_enable then
		build_calendar(s, clock, {
			calendar_use_menu_theme = calendar_use_menu_theme,
			bar_position = bar_position,
		})
	end

	-- ---------------------------------------------------------------------
	-- Click Action
	-- ---------------------------------------------------------------------

	if app then
		clock:buttons(gears.table.join(awful.button({}, 1, function()
			awful.spawn(app, false)
		end)))
	end

	-- ---------------------------------------------------------------------
	-- Widget
	-- ---------------------------------------------------------------------

	local placed = wibox.widget({
		clock,
		widget = wibox.container.place,
		halign = "center",
		valign = "center",
	})

	local with_margin = wibox.widget({
		placed,
		left = pad_h,
		right = pad_h,
		top = pad_v,
		bottom = pad_v,
		widget = wibox.container.margin,
	})

	local bg_block = wibox.widget({
		with_margin,
		fg = fg,
		bg = bg,
		widget = wibox.container.background,
	})

	return wibox.container.constraint(bg_block, "exact", nil, bar_height)
end

return M
