-- ~/.config/awesome/shell/bar/wibar/sections.lua
local beautiful = require("beautiful")
local wibox = require("wibox")

local M = {}

local runtime = {
	ctx = {},
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function ctx()
	return runtime.ctx or {}
end

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
		gap = gap + 16
	end

	return gap
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(args)
	runtime.ctx = args and (args.ctx or args) or {}
	return M
end

function M.build(args)
	args = args or {}

	local show_start = (args.show_start == true)
	local show_tags = (args.show_tags == true)
	local show_notify = (args.show_notify == true)

	local start_btn = args.start_btn or empty_widget()
	local tags = args.tags or { indicator = empty_widget() }
	local tabs = args.tabs or { tasklist = empty_widget() }
	local tray = args.tray or empty_widget()
	local notify = args.notify or empty_widget()
	local clock = args.clock or empty_widget()
	local layoutbox = args.layoutbox or empty_widget()

	local tabs_leading_spacer = hspace(compute_tabs_leading_gap(show_start, show_tags))

	local parts = {
		start_btn = start_btn,
		tags = tags,
		tabs = tabs,
		tray = tray,
		notify = notify,
		clock = clock,
		layoutbox = layoutbox,
		tabs_leading_spacer = tabs_leading_spacer,
		spacing_l = 0,
		spacing_r = 0,
	}

	local notify_gap_left = beautiful.notify_button_gap_left or 0
	local notify_zone_width = beautiful.notify_button_zone_width or 0
	local notify_seam_offset = beautiful.notify_button_seam_offset or 0
	local systray_bg = beautiful.systray_bg or beautiful.bg_systray or beautiful.wibar_bg
	local bar_height = beautiful.wibar_height or 0

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

	return {
		left = {
			layout = wibox.layout.fixed.horizontal,
			spacing = parts.spacing_l,
			parts.start_btn,
			parts.tags.indicator,
			parts.tabs_leading_spacer,
			parts.tabs.tasklist,
		},
		center = nil,
		right = {
			layout = wibox.layout.fixed.horizontal,
			spacing = parts.spacing_r,
			parts.layoutbox,
			notify_with_gap,
			parts.clock,
		},
	}
end

return M
