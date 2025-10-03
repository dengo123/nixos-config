-- ui/theme/wibar.lua
local beautiful = require("beautiful")
local xr = require("beautiful.xresources")
local dpi = xr.apply_dpi

local M = {}

-- 🔒 Modul-lokale Helpers (theme-spezifisch)
local function pick_allowed(src, allow)
	local out = {}
	if not src then
		return out
	end
	for k, v in pairs(allow or {}) do
		if src[k] ~= nil then
			out[k] = src[k]
		end
	end
	return out
end

-- Was darf extern überschrieben werden (nur Abstände/Größen, keine Marke/Farbe)?
local ALLOW = {
	wibar_height = true,
	wibar_item_pad_h = true,
	wibar_item_pad_v = true,
	layoutbox_pad_h = true,
	layoutbox_pad_v = true,
}

function M.init(cfg)
	local C = cfg.colors
	local H = cfg.helpers -- <- generisch (ui/helpers.lua)

	-- 1) externe (erlaubte) Overrides einsammeln
	local ext = pick_allowed(cfg.wibar or {}, ALLOW)

	-- 2) Quelle der Wahrheit setzen
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

	beautiful.systray_bg = C.blue_light
	beautiful.systray_fg = beautiful.wibar_fg
	beautiful.systray_pad_h = dpi(6)
	beautiful.systray_pad_v = 0
	beautiful.systray_base_size = dpi(18)
	beautiful.bg_systray = beautiful.systray_bg

	beautiful.clock_bg = beautiful.systray_bg
	beautiful.clock_fg = beautiful.wibar_fg
	beautiful.clock_pad_h = dpi(12)
	beautiful.clock_pad_v = 0
	beautiful.clock_format = "%H:%M"
	beautiful.clock_refresh = 1

	beautiful.clock_calendar_enable = true
	beautiful.clock_calendar_use_menu_theme = false
	beautiful.clock_calendar_placement = "bottom_right"
	beautiful.clock_calendar_bg = C.creme
	beautiful.clock_calendar_fg = C.black
	beautiful.clock_calendar_focus = C.creme_focus
	beautiful.clock_calendar_border_color = C.black
	beautiful.clock_calendar_border_width = 0

	beautiful.layoutbox_pad_h = ext.layoutbox_pad_h or dpi(6)
	beautiful.layoutbox_pad_v = ext.layoutbox_pad_v or 0

	-- 3) abschließend sperren (generischer Helper)
	H.lock_beautiful_keys({
		"wibar_position",
		"wibar_height",
		"wibar_bg",
		"wibar_fg",
		"wibar_on_top",
		"wibar_opacity",
		"wibar_shape",
		"wibar_margins",
		"wibar_item_pad_h",
		"wibar_item_pad_v",
		"systray_bg",
		"systray_fg",
		"systray_pad_h",
		"systray_pad_v",
		"systray_base_size",
		"bg_systray",
		"clock_bg",
		"clock_fg",
		"clock_pad_h",
		"clock_pad_v",
		"clock_format",
		"clock_refresh",
		"clock_calendar_enable",
		"clock_calendar_use_menu_theme",
		"clock_calendar_placement",
		"clock_calendar_bg",
		"clock_calendar_fg",
		"clock_calendar_focus",
		"clock_calendar_border_color",
		"clock_calendar_border_width",
		"layoutbox_pad_h",
		"layoutbox_pad_v",
	}, "error")
end

return M
