-- ~/.config/awesome/theme/wibar.lua
-- Quelle der Wahrheit für Bar/Wibar + Clock + Systray (hart gesetzt & gesperrt)

local beautiful = require("beautiful")
local xr = require("beautiful.xresources")
local dpi = xr.apply_dpi

local M = {}

-- kleine Helper-Funktion: sperrt definierte beautiful-Keys gegen spätere Änderungen
local function lock_beautiful_keys(keys, mode)
	-- mode: "error" | "warn" | "silent"
	mode = mode or "error"
	local naughty_ok, naughty = pcall(require, "naughty")

	local mt = getmetatable(beautiful)
	if not mt then
		mt = {}
		debug.setmetatable(beautiful, mt)
	end

	-- initiale Werte merken
	local INIT, LOCK = {}, {}
	for _, k in ipairs(keys) do
		INIT[k] = rawget(beautiful, k)
		LOCK[k] = true
	end

	local prev_newindex = mt.__newindex
	mt.__newindex = function(t, key, val)
		if LOCK[key] and val ~= INIT[key] then
			local msg = ("Attempt to modify locked theme key: beautiful.%s"):format(key)
			if mode == "error" then
				error(msg, 2)
			elseif mode == "warn" and naughty_ok then
				naughty.notify({ title = "Theme lock", text = msg })
			end
			return
		end
		return prev_newindex and prev_newindex(t, key, val) or rawset(t, key, val)
	end
end

function M.init()
	---------------------------------------------------------------------------
	-- Wibar (Bar) – feste Werte
	---------------------------------------------------------------------------
	beautiful.wibar_position = "bottom" -- "top" | "bottom" | "left" | "right"
	beautiful.wibar_height = dpi(32)
	beautiful.wibar_bg = "#235CDB"
	beautiful.wibar_fg = "#FFFFFF"
	beautiful.wibar_on_top = false
	beautiful.wibar_opacity = 1.0
	beautiful.wibar_shape = nil -- z.B. gears.shape.rounded_rect
	beautiful.wibar_margins = { top = 0, right = 0, bottom = 0, left = 0 }

	-- generisches Item-Padding (z. B. für Layoutbox-Margins)
	beautiful.wibar_item_pad_h = dpi(8)
	beautiful.wibar_item_pad_v = dpi(2)

	---------------------------------------------------------------------------
	-- Systray – feste Werte
	---------------------------------------------------------------------------
	beautiful.systray_bg = "#0B89E7"
	beautiful.systray_fg = beautiful.wibar_fg
	beautiful.systray_pad_h = dpi(6)
	beautiful.systray_pad_v = 0
	beautiful.systray_base_size = dpi(18) -- feste Zielgröße; nicht an Bar-Höhe koppeln
	beautiful.bg_systray = beautiful.systray_bg -- Kompat-Alias

	---------------------------------------------------------------------------
	-- Clock – feste Werte (Clock.build() liest diese Keys)
	---------------------------------------------------------------------------
	beautiful.clock_bg = beautiful.systray_bg
	beautiful.clock_fg = beautiful.wibar_fg
	beautiful.clock_pad_h = dpi(12)
	beautiful.clock_pad_v = 0
	beautiful.clock_edge_left = 0
	beautiful.clock_edge_right = 0
	beautiful.clock_format = "%H:%M"
	beautiful.clock_refresh = 1

	-- Kalender-Popup (Borders standardmäßig aus)
	beautiful.clock_calendar_enable = true
	beautiful.clock_calendar_use_menu_theme = false
	beautiful.clock_calendar_placement = "bottom_right" -- "top_left|top_right|bottom_left|bottom_right"
	beautiful.clock_calendar_bg = "#FFF7E6"
	beautiful.clock_calendar_fg = "#000000"
	beautiful.clock_calendar_focus = "#F2E7CF"
	beautiful.clock_calendar_border_color = "#000000"
	beautiful.clock_calendar_border_width = 0

	---------------------------------------------------------------------------
	-- Layoutbox – Abstände
	---------------------------------------------------------------------------
	beautiful.layoutbox_pad_h = dpi(6)
	beautiful.layoutbox_pad_v = 0

	---------------------------------------------------------------------------
	-- Keys sperren (nicht überschreibbar)
	---------------------------------------------------------------------------
	lock_beautiful_keys({
		-- Wibar
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

		-- Systray
		"systray_bg",
		"systray_fg",
		"systray_pad_h",
		"systray_pad_v",
		"systray_base_size",
		"bg_systray",

		-- Clock
		"clock_bg",
		"clock_fg",
		"clock_pad_h",
		"clock_pad_v",
		"clock_edge_left",
		"clock_edge_right",
		"clock_format",
		"clock_refresh",

		-- Calendar
		"clock_calendar_enable",
		"clock_calendar_use_menu_theme",
		"clock_calendar_placement",
		"clock_calendar_bg",
		"clock_calendar_fg",
		"clock_calendar_focus",
		"clock_calendar_border_color",
		"clock_calendar_border_width",

		-- Layoutbox
		"layoutbox_pad_h",
		"layoutbox_pad_v",
	}, "error") -- "warn" oder "silent" möglich
end

return M
