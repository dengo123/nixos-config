-- ~/.config/awesome/input/client/tags.lua
local awful = require("awful")

local M = {}

-- =========================================================================
-- Helpers
-- =========================================================================

local function kbd_intent(ms)
	awesome.emit_signal("focus_policy::keyboard_intent", ms or 250)
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.build(modkey, actions)
	assert(type(actions) == "table", "input.client.tags: actions fehlt/ungueltig")

	return awful.util.table.join(
		awful.key({ modkey, "Control", "Shift" }, "Right", function()
			kbd_intent()
			actions.move_client_to_neighbor_tag(1, true)
		end, {
			description = "Move Window To Next Tag",
			group = "Client",
		}),

		awful.key({ modkey, "Control", "Shift" }, "Left", function()
			kbd_intent()
			actions.move_client_to_neighbor_tag(-1, true)
		end, {
			description = "Move Window To Previous Tag",
			group = "Client",
		})
	)
end

return function(modkey, actions)
	return M.build(modkey, actions)
end
