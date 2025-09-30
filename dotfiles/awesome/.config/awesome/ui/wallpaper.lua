-- ~/.config/awesome/ui/wallpaper.lua
local gears = require("gears")
local beautiful = require("beautiful")

local M = {}

-- Setzt das Wallpaper für einen Screen
function M.set(s)
	if beautiful.wallpaper then
		local wp = beautiful.wallpaper
		if type(wp) == "function" then
			wp = wp(s)
		end
		gears.wallpaper.maximized(wp, s, true)
	end
end

-- Hängt das Wallpaper-Update an Screen-Events (z. B. Auflösungsänderung)
function M.hook()
	screen.connect_signal("property::geometry", M.set)
end

return M
