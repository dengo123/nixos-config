-- ~/.config/awesome/shell/bar/themes/wibar.lua
local beautiful = require("beautiful")
local xr = require("beautiful.xresources")

local dpi = xr.apply_dpi

local M = {}

-- =========================================================================
-- Helpers
-- =========================================================================

local function resolved_theme(args)
	local ui = (args and args.ui) or {}
	return ui.theme or {}
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(args)
	args = args or {}

	local theme = resolved_theme(args)
	local C = theme.colors or {}
	local F = theme.fonts or {}

	assert(type(C.primary) == "string" and C.primary ~= "", "bar.theme.wibar: theme.colors.primary fehlt")
	assert(type(C.secondary) == "string" and C.secondary ~= "", "bar.theme.wibar: theme.colors.secondary fehlt")
	assert(type(C.tertiary) == "string" and C.tertiary ~= "", "bar.theme.wibar: theme.colors.tertiary fehlt")
	assert(type(C.surface) == "string" and C.surface ~= "", "bar.theme.wibar: theme.colors.surface fehlt")
	assert(
		type(C.surface_focus) == "string" and C.surface_focus ~= "",
		"bar.theme.wibar: theme.colors.surface_focus fehlt"
	)
	assert(type(C.black) == "string" and C.black ~= "", "bar.theme.wibar: theme.colors.black fehlt")
	assert(type(C.white) == "string" and C.white ~= "", "bar.theme.wibar: theme.colors.white fehlt")
	assert(type(F.ui) == "string" and F.ui ~= "", "bar.theme.wibar: theme.fonts.ui fehlt")
	assert(type(F.ui_bold) == "string" and F.ui_bold ~= "", "bar.theme.wibar: theme.fonts.ui_bold fehlt")

	-- ---------------------------------------------------------------------
	-- Wibar
	-- ---------------------------------------------------------------------

	beautiful.wibar_height = dpi(32)
	beautiful.wibar_bg = C.primary
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

	beautiful.systray_bg = C.secondary
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

	beautiful.clock_calendar_bg = C.surface
	beautiful.clock_calendar_fg = C.black
	beautiful.clock_calendar_border_color = C.surface_focus
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
		font = F.ui,
	}

	-- ---------------------------------------------------------------------
	-- Notify Button
	-- ---------------------------------------------------------------------

	beautiful.notify_button_size = dpi(20)
	beautiful.notify_button_radius = dpi(10)
	beautiful.notify_button_pad_h = dpi(0)
	beautiful.notify_button_pad_v = dpi(0)
	beautiful.notify_button_offset_y = dpi(1)

	beautiful.notify_button_bg = beautiful.systray_bg
	beautiful.notify_button_bg_hover = C.primary
	beautiful.notify_button_fg = beautiful.wibar_fg

	beautiful.notify_button_border_color = C.tertiary
	beautiful.notify_button_border_width = dpi(1)

	beautiful.notify_button_font = F.ui_bold
	beautiful.notify_button_badge_font = F.ui_bold

	beautiful.notify_button_glyph_closed = "⮜"
	beautiful.notify_button_glyph_open = "⮝"
	beautiful.notify_button_glyph_offset_y = 5

	beautiful.notify_button_gap_left = dpi(16)
	beautiful.notify_button_zone_width = dpi(12)
	beautiful.notify_button_seam_offset = dpi(11)
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
