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

local function open_run(launchers_open)
	if launchers_open and type(launchers_open.run) == "function" then
		launchers_open.run({
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
