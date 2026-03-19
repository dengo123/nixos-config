-- ~/.config/awesome/input/keys/global/run.lua
local awful = require("awful")
local gears = require("gears")

return function(modkey, launchers)
	local function open_run()
		if launchers and launchers.open and launchers.open.run then
			launchers.open.run({
				screen = mouse and mouse.screen or nil,
				mode = "run",
			})
		end
	end

	return gears.table.join(awful.key({ modkey }, "space", function()
		open_run()
	end, { description = "open run-launcher", group = "launchers" }))
end
