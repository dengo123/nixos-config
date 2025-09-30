-- ~/.config/awesome/ui/theme/wibar.lua
local beautiful = require("beautiful")
local W = {}

function W.init(_cfg)
	beautiful.wibar_height = beautiful.wibar_height or 28
	beautiful.wibar_bg = beautiful.wibar_bg or "#235CDB"
	beautiful.wibar_fg = beautiful.wibar_fg or "#FFFFFF"

	-- Einheitliche Innenabstände für ALLE Bar-Elemente
	beautiful.wibar_item_pad_h = beautiful.wibar_item_pad_h or 8
	beautiful.wibar_item_pad_v = beautiful.wibar_item_pad_v or 4
end

return W
