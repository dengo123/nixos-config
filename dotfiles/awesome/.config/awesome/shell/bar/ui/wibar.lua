-- ~/.config/awesome/shell/bar/ui/wibar.lua
local beautiful = require("beautiful")
local xr = require("beautiful.xresources")

local dpi = xr.apply_dpi

local M = {}

local runtime = {
	ui = {},
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function resolved_theme()
	local ui = runtime.ui or {}
	return ui.theme or {}
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(opts)
	opts = opts or {}
	runtime.ui = opts.ui or runtime.ui or {}

	local theme = resolved_theme()
	local colors = theme.colors or {}
	local fonts = theme.fonts or {}

	-- ---------------------------------------------------------------------
	-- Wibar
	-- ---------------------------------------------------------------------

	beautiful.wibar_height = dpi(32)
	beautiful.wibar_bg = colors.primary
	beautiful.wibar_fg = colors.text_invert
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

	beautiful.systray_bg = colors.secondary
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

	beautiful.clock_calendar_bg = colors.surface
	beautiful.clock_calendar_fg = colors.text
	beautiful.clock_calendar_border_color = colors.surface_focus
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
		font = fonts.ui,
	}

	-- ---------------------------------------------------------------------
	-- Notify Button
	-- ---------------------------------------------------------------------

	beautiful.notify_button_size = dpi(20)
	beautiful.notify_button_radius = dpi(10)
	beautiful.notify_button_pad_h = dpi(0)
	beautiful.notify_button_pad_v = dpi(0)
	beautiful.notify_button_offset_x = dpi(1)
	beautiful.notify_button_offset_y = dpi(1)

	beautiful.notify_button_bg = beautiful.systray_bg
	beautiful.notify_button_bg_hover = colors.primary
	beautiful.notify_button_fg = beautiful.wibar_fg

	beautiful.notify_button_border_color = colors.tertiary
	beautiful.notify_button_border_width = dpi(1)

	beautiful.notify_button_font = fonts.ui_bold
	beautiful.notify_button_badge_font = fonts.ui_bold

	beautiful.notify_button_glyph_closed = "⮜"
	beautiful.notify_button_glyph_open = "⮝"
	beautiful.notify_button_glyph_offset_y = 1

	beautiful.notify_button_gap_left = dpi(16)
	beautiful.notify_button_zone_width = dpi(12)
	beautiful.notify_button_seam_offset = dpi(11)

	return M
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
