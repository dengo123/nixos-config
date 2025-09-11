-- ~/.config/awesome/features/workspaces/screen_signals.lua
local gears = require("gears")

local M = {}

function M.attach(wallpaper_fn)
	if wallpaper_fn then
		screen.connect_signal("property::geometry", function(s)
			wallpaper_fn(s)
		end)
	end
end

return M
