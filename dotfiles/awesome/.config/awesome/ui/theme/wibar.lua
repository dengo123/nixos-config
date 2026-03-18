-- ~/.config/awesome/ui/theme/wibar.lua
local beautiful = require("beautiful")
local xr = require("beautiful.xresources")

local dpi = xr.apply_dpi

local M = {}

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(cfg)
	cfg = cfg or {}

	-- ---------------------------------------------------------------------
	-- Colors
	-- ---------------------------------------------------------------------

	local C = cfg.colors or require("ui.colors").get()

	-- ---------------------------------------------------------------------
	-- Wibar
	-- ---------------------------------------------------------------------

	beautiful.wibar_height = dpi(32)
	beautiful.wibar_bg = C.blue_luna
	beautiful.wibar_fg = C.white
	beautiful.wibar_on_top = false
	beautiful.wibar_opacity = 1.0
	beautiful.wibar_shape = nil
	beautiful.wibar_margins = {
		top = 0,
		right = 0,
		bottom = 0,
		left = 0,
	}

	beautiful.wibar_item_pad_h = dpi(8)
	beautiful.wibar_item_pad_v = dpi(2)

	-- ---------------------------------------------------------------------
	-- Systray
	-- ---------------------------------------------------------------------

	beautiful.systray_bg = C.blue_light
	beautiful.systray_fg = beautiful.wibar_fg
	beautiful.systray_pad_h = dpi(6)
	beautiful.systray_pad_v = 0
	beautiful.systray_base_size = dpi(18)
	beautiful.bg_systray = beautiful.systray_bg

	-- ---------------------------------------------------------------------
	-- Clock
	-- ---------------------------------------------------------------------

	beautiful.clock_bg = beautiful.systray_bg
	beautiful.clock_fg = beautiful.wibar_fg
	beautiful.clock_pad_h = dpi(12)
	beautiful.clock_pad_v = 0

	-- ---------------------------------------------------------------------
	-- Calendar
	-- ---------------------------------------------------------------------

	beautiful.clock_calendar_bg = C.creme
	beautiful.clock_calendar_fg = C.black
	beautiful.clock_calendar_focus = C.creme_focus
	beautiful.clock_calendar_border_color = C.black
	beautiful.clock_calendar_border_width = 0

	-- ---------------------------------------------------------------------
	-- Layoutbox
	-- ---------------------------------------------------------------------

	beautiful.layoutbox_pad_h = dpi(6)
	beautiful.layoutbox_pad_v = 0

	-- ---------------------------------------------------------------------
	-- Tags Indicator
	-- ---------------------------------------------------------------------

	beautiful.tags_indicator = {
		pad_h = dpi(8),
		pad_v = dpi(0),
		collapsed_pad_h = dpi(6),
		fmt = "%d",
		font = "Sans 10",
	}

	-- ---------------------------------------------------------------------
	-- Notify Button
	-- ---------------------------------------------------------------------

	beautiful.notify_button_size = dpi(22)
	beautiful.notify_button_radius = dpi(11)
	beautiful.notify_button_pad_h = dpi(0)
	beautiful.notify_button_pad_v = dpi(0)

	beautiful.notify_button_bg = beautiful.systray_bg
	beautiful.notify_button_bg_hover = C.blue_luna
	beautiful.notify_button_fg = beautiful.wibar_fg

	beautiful.notify_button_border_color = C.blue_dark
	beautiful.notify_button_border_width = dpi(1)

	beautiful.notify_button_font = "Sans Bold 11"
	beautiful.notify_button_badge_font = "Sans Bold 10"

	beautiful.notify_button_glyph_closed = "▾"
	beautiful.notify_button_glyph_open = "▴"

	beautiful.notify_button_gap_left = dpi(16)
	beautiful.notify_button_zone_width = dpi(12)
	beautiful.notify_button_seam_offset = dpi(12)
end

function M.props()
	return {
		height = beautiful.wibar_height,
		bg = beautiful.wibar_bg,
		fg = beautiful.wibar_fg,
		on_top = beautiful.wibar_on_top,
		opacity = beautiful.wibar_opacity,
		shape = beautiful.wibar_shape,
		margins = beautiful.wibar_margins,
	}
end

return M
