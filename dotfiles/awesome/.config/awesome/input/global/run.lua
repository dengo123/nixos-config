-- ~/.config/awesome/input/global/run.lua
local awful = require("awful")
local gears = require("gears")

local M = {}

-- =========================================================================
-- Helpers
-- =========================================================================

local function current_screen()
	return (mouse and mouse.screen) or nil
end

local function open_run(launchers)
	if launchers and launchers.open and launchers.open.run then
		launchers.open.run({
			screen = current_screen(),
			mode = "run",
		})
	end
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.build(modkey, launchers)
	return gears.table.join(awful.key({ modkey }, "space", function()
		open_run(launchers)
	end, {
		description = "Open Run Launcher",
		group = "Launchers",
	}))
end

return function(modkey, launchers)
	return M.build(modkey, launchers)
end
