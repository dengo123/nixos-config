-- ~/.config/awesome/system/autorun.lua
local awful = require("awful")
local M = {}
local function once(cmd)
	awful.spawn.once(cmd)
end
function M.start()
	once("nm-applet")
	once("blueman-applet")
	once("pasystray")
end

return M
