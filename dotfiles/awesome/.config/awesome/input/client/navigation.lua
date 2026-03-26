-- ~/.config/awesome/input/client/navigation.lua
local awful = require("awful")

local M = {}

-- =========================================================================
-- Public API
-- =========================================================================

function M.build(modkey, actions)
	actions = actions or {}

	local focus_client = actions.focus_client or function(_) end
	local swap_client = actions.swap_client or function(_) end

	return awful.util.table.join(
		awful.key({ modkey }, "Right", function()
			focus_client("right")
		end, {
			description = "Focus Client Right",
			group = "Client",
		}),

		awful.key({ modkey }, "Left", function()
			focus_client("left")
		end, {
			description = "Focus Client Left",
			group = "Client",
		}),

		awful.key({ modkey }, "Up", function()
			focus_client("up")
		end, {
			description = "Focus Client Up",
			group = "Client",
		}),

		awful.key({ modkey }, "Down", function()
			focus_client("down")
		end, {
			description = "Focus Client Down",
			group = "Client",
		}),

		awful.key({ modkey, "Shift" }, "Right", function()
			swap_client("right")
		end, {
			description = "Move Window Right",
			group = "Client",
		}),

		awful.key({ modkey, "Shift" }, "Left", function()
			swap_client("left")
		end, {
			description = "Move Window Left",
			group = "Client",
		}),

		awful.key({ modkey, "Shift" }, "Up", function()
			swap_client("up")
		end, {
			description = "Move Window Up",
			group = "Client",
		}),

		awful.key({ modkey, "Shift" }, "Down", function()
			swap_client("down")
		end, {
			description = "Move Window Down",
			group = "Client",
		})
	)
end

return function(modkey, actions)
	return M.build(modkey, actions)
end
