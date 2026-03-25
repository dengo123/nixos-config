-- ~/.config/awesome/input/global/menu.lua
local awful = require("awful")

local M = {}

function M.build(modkey, _cfg)
	return awful.util.table.join(awful.key({ modkey }, "Home", function()
		awesome.emit_signal("menu::toggle")
	end, {
		description = "toggle start menu",
		group = "Launchers",
	}))
end

return function(modkey, cfg)
	return M.build(modkey, cfg)
end
