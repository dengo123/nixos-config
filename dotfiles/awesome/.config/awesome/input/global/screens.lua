-- ~/.config/awesome/input/global/screens.lua
local awful = require("awful")

local M = {}

-- =========================================================================
-- Helpers
-- =========================================================================

local function kbd_intent(ms)
	awesome.emit_signal("focus_policy::keyboard_intent", ms or 250)
end

local function focus_screen_in_dir(actions, dir)
	local target = actions.scr_in_dir(dir)
	if target then
		kbd_intent()
		awful.screen.focus(target)
	end
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.build(modkey, actions)
	assert(type(actions) == "table", "input.global.screens: actions fehlt/ungueltig")

	return awful.util.table.join(
		awful.key({ modkey, "Mod1" }, "Left", function()
			focus_screen_in_dir(actions, "left")
		end, {
			description = "Focus Screen Left",
			group = "Screens",
		}),

		awful.key({ modkey, "Mod1" }, "Right", function()
			focus_screen_in_dir(actions, "right")
		end, {
			description = "Focus Screen Right",
			group = "Screens",
		}),

		awful.key({ modkey, "Mod1" }, "Up", function()
			focus_screen_in_dir(actions, "up")
		end, {
			description = "Focus Screen Up",
			group = "Screens",
		}),

		awful.key({ modkey, "Mod1" }, "Down", function()
			focus_screen_in_dir(actions, "down")
		end, {
			description = "Focus Screen Down",
			group = "Screens",
		})
	)
end

return function(modkey, actions)
	return M.build(modkey, actions)
end
