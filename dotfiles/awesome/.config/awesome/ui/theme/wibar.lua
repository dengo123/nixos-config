-- ~/.config/awesome/ui/theme/wibar.lua
local beautiful = require("beautiful")
local xr = require("beautiful.xresources")
local dpi = xr.apply_dpi

local M = {}

-- erlaubte Overrides (nur Maße/Abstände/Format)
local ALLOW = {
	wibar_height = true,
	wibar_item_pad_h = true,
	wibar_item_pad_v = true,
	layoutbox_pad_h = true,
	layoutbox_pad_v = true,

	-- Tags-Indicator
	tags_indicator_pad_h = true,
	tags_indicator_pad_v = true,
	tags_indicator_collapsed_pad_h = true,
	tags_indicator_collapsed_min_width = true,
	tags_indicator_fmt = true,
	tags_indicator_font = true,
}

local function pick_allowed(src, allow)
	local out = {}
	if not src then
		return out
	end
	for k, _ in pairs(allow or {}) do
		if src[k] ~= nil then
			out[k] = src[k]
		end
	end
	return out
end

function M.init(cfg)
	cfg = cfg or {}
	local C = cfg.colors
	local ext = pick_allowed(cfg.wibar or {}, ALLOW)

	-- Wibar
	beautiful.wibar_position = "bottom"
	beautiful.wibar_height = ext.wibar_height or dpi(32)
	beautiful.wibar_bg = C.blue_luna
	beautiful.wibar_fg = C.white
	beautiful.wibar_on_top = false
	beautiful.wibar_opacity = 1.0
	beautiful.wibar_shape = nil
	beautiful.wibar_margins = { top = 0, right = 0, bottom = 0, left = 0 }

	beautiful.wibar_item_pad_h = ext.wibar_item_pad_h or dpi(8)
	beautiful.wibar_item_pad_v = ext.wibar_item_pad_v or dpi(2)

	-- Systray
	beautiful.systray_bg = C.blue_light
	beautiful.systray_fg = beautiful.wibar_fg
	beautiful.systray_pad_h = dpi(6)
	beautiful.systray_pad_v = 0
	beautiful.systray_base_size = dpi(18)
	beautiful.bg_systray = beautiful.systray_bg

	-- Clock
	beautiful.clock_bg = beautiful.systray_bg
	beautiful.clock_fg = beautiful.wibar_fg
	beautiful.clock_pad_h = dpi(12)
	beautiful.clock_pad_v = 0
	beautiful.clock_format = "%H:%M"
	beautiful.clock_refresh = 1

	-- Calendar popup
	beautiful.clock_calendar_enable = true
	beautiful.clock_calendar_use_menu_theme = false
	beautiful.clock_calendar_placement = "bottom_right"
	beautiful.clock_calendar_bg = C.creme
	beautiful.clock_calendar_fg = C.black
	beautiful.clock_calendar_focus = C.creme_focus
	beautiful.clock_calendar_border_color = C.black
	beautiful.clock_calendar_border_width = 0

	-- Layoutbox
	beautiful.layoutbox_pad_h = ext.layoutbox_pad_h or dpi(6)
	beautiful.layoutbox_pad_v = ext.layoutbox_pad_v or 0

	-- Tags-Indicator (für shell/bar/widgets/tags.lua)
	local TI = {
		pad_h = dpi(8),
		pad_v = dpi(0),
		collapsed_pad_h = dpi(6),
		collapsed_min_width = nil, -- optional: feste Mindestbreite im kollabierten Zustand
		fmt = "%d",
		font = beautiful.font,
	}
	-- Apply overrides aus cfg.wibar
	TI.pad_h = ext.tags_indicator_pad_h or TI.pad_h
	TI.pad_v = ext.tags_indicator_pad_v or TI.pad_v
	TI.collapsed_pad_h = ext.tags_indicator_collapsed_pad_h or TI.collapsed_pad_h
	TI.collapsed_min_width = ext.tags_indicator_collapsed_min_width or TI.collapsed_min_width
	TI.fmt = ext.tags_indicator_fmt or TI.fmt
	TI.font = ext.tags_indicator_font or TI.font

	beautiful.tags_indicator = TI
end

return M
