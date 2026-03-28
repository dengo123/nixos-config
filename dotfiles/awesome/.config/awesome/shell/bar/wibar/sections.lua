-- ~/.config/awesome/shell/bar/wibar/sections.lua
local beautiful = require("beautiful")
local wibox = require("wibox")

local M = {}

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
		gap = gap + 16
	end

	return gap
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.build(opts)
	opts = opts or {}

	local show_start = (opts.show_start == true)
	local show_tags = (opts.show_tags == true)
	local show_notify = (opts.show_notify == true)

	local start_btn = opts.start_btn or empty_widget()
	local tags = opts.tags or { indicator = empty_widget() }
	local tabs = opts.tabs or { tasklist = empty_widget() }
	local tray = opts.tray or empty_widget()
	local notify = opts.notify or empty_widget()
	local clock = opts.clock or empty_widget()
	local layoutbox = opts.layoutbox or empty_widget()

	local tabs_leading_spacer = hspace(compute_tabs_leading_gap(show_start, show_tags))

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
				notify,
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
			tray,
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
			spacing = 0,
			start_btn,
			tags.indicator,
			tabs_leading_spacer,
			tabs.tasklist,
		},
		center = nil,
		right = {
			layout = wibox.layout.fixed.horizontal,
			spacing = 0,
			layoutbox,
			notify_with_gap,
			clock,
		},
	}
end

return M
