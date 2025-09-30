-- ~/.config/awesome/ui/theme/wibar.lua
local beautiful = require("beautiful")
local W = {}

function W.init(_cfg)
	beautiful.wibar_height = beautiful.wibar_height or 28
	beautiful.wibar_bg = beautiful.wibar_bg or "#235CDB"
	beautiful.wibar_fg = beautiful.wibar_fg or "#FFFFFF"

	beautiful.wibar_item_pad_h = beautiful.wibar_item_pad_h or 8
	beautiful.wibar_item_pad_v = beautiful.wibar_item_pad_v or 4

	-- Systray-Defaults
	beautiful.systray_pad_h = beautiful.systray_pad_h or 4
	beautiful.systray_pad_v = beautiful.systray_pad_v or 0

	-- WICHTIG: Systray nimmt diese Farbe als eigenen Hintergrund
	beautiful.bg_systray = beautiful.bg_systray or beautiful.wibar_bg

	-- sinnvolle Icongröße (bändigt „fette“ Applets)
	local h = tonumber(beautiful.wibar_height) or 28
	beautiful.systray_base_size = beautiful.systray_base_size or math.max(1, h - 6)
end

return W
