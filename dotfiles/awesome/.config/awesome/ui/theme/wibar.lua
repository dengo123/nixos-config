-- ~/.config/awesome/ui/theme/wibar.lua
local beautiful = require("beautiful")
local W = {}

function W.init(_cfg)
	beautiful.wibar_height = beautiful.wibar_height or 32
	beautiful.wibar_bg = beautiful.wibar_bg or "#235CDB"
	beautiful.wibar_fg = beautiful.wibar_fg or "#FFFFFF"

	beautiful.wibar_item_pad_h = beautiful.wibar_item_pad_h or 8
	beautiful.wibar_item_pad_v = beautiful.wibar_item_pad_v or 2

	-- Systray
	beautiful.systray_pad_h = beautiful.systray_pad_h or 4
	beautiful.systray_pad_v = beautiful.systray_pad_v or 0
	beautiful.systray_bg = beautiful.systray_bg or "#0B89E7"
	beautiful.bg_systray = beautiful.bg_systray or beautiful.systray_bg
	local h = tonumber(beautiful.wibar_height) or 28
	beautiful.systray_base_size = beautiful.systray_base_size or math.max(1, h - 6)

	-- Clock: gleiche Optik wie Systray
	beautiful.clock_bg = beautiful.clock_bg or beautiful.systray_bg
	beautiful.clock_pad_h = beautiful.clock_pad_h or 6
	beautiful.clock_pad_v = beautiful.clock_pad_v or 0
end

return W
