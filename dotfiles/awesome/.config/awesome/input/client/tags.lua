-- ~/.config/awesome/input/client/tags.lua
local awful = require("awful")
local gears = require("gears")

local M = {}

-- =========================================================================
-- Helpers
-- =========================================================================

local function kbd_intent(ms)
	awesome.emit_signal("focus_policy::keyboard_intent", ms or 250)
end

local function has_client_tag_actions(actions)
	return type(actions) == "table" and type(actions.move_client_to_neighbor_tag) == "function"
end

local function call(actions, ...)
	if not has_client_tag_actions(actions) then
		return
	end

	kbd_intent()
	actions.move_client_to_neighbor_tag(...)
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.build(modkey, actions)
	if not has_client_tag_actions(actions) then
		return gears.table.join()
	end

	return gears.table.join(
		awful.key({ modkey, "Control", "Shift" }, "Right", function()
			call(actions, 1, true)
		end, {
			description = "Move Window To Next Tag",
			group = "Client",
		}),

		awful.key({ modkey, "Control", "Shift" }, "Left", function()
			call(actions, -1, true)
		end, {
			description = "Move Window To Previous Tag",
			group = "Client",
		})
	)
end

return function(modkey, actions)
	return M.build(modkey, actions)
end
