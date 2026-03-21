-- ~/.config/awesome/input/client/screens.lua
local awful = require("awful")

local M = {}

-- =========================================================================
-- Helpers
-- =========================================================================

local function kbd_intent(ms)
	awesome.emit_signal("focus_policy::keyboard_intent", ms or 250)
end

local function move_client_to_screen(actions, dir)
	kbd_intent()
	actions.move_client_to_screen(dir)
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.build(modkey, actions)
	assert(type(actions) == "table", "input.client.screens: actions fehlt/ungueltig")

	return awful.util.table.join(
		awful.key({ modkey, "Shift", "Mod1" }, "Left", function()
			move_client_to_screen(actions, "left")
		end, {
			description = "Move Window To Screen Left",
			group = "Client",
		}),

		awful.key({ modkey, "Shift", "Mod1" }, "Right", function()
			move_client_to_screen(actions, "right")
		end, {
			description = "Move Window To Screen Right",
			group = "Client",
		}),

		awful.key({ modkey, "Shift", "Mod1" }, "Up", function()
			move_client_to_screen(actions, "up")
		end, {
			description = "Move Window To Screen Up",
			group = "Client",
		}),

		awful.key({ modkey, "Shift", "Mod1" }, "Down", function()
			move_client_to_screen(actions, "down")
		end, {
			description = "Move Window To Screen Down",
			group = "Client",
		})
	)
end

return function(modkey, actions)
	return M.build(modkey, actions)
end
