-- ~/.config/awesome/input/client/navigation.lua
local awful = require("awful")
local gears = require("gears")

local M = {}

-- =========================================================================
-- Helpers
-- =========================================================================

local function has_client_navigation(actions)
	return type(actions) == "table"
		and (type(actions.focus_client) == "function" or type(actions.swap_client) == "function")
end

local function call(actions, name, ...)
	if not (actions and type(actions[name]) == "function") then
		return
	end

	actions[name](...)
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.build(modkey, actions)
	if not has_client_navigation(actions) then
		return gears.table.join()
	end

	return gears.table.join(
		awful.key({ modkey }, "Right", function()
			call(actions, "focus_client", "right")
		end, {
			description = "Focus Client Right",
			group = "Client",
		}),

		awful.key({ modkey }, "Left", function()
			call(actions, "focus_client", "left")
		end, {
			description = "Focus Client Left",
			group = "Client",
		}),

		awful.key({ modkey }, "Up", function()
			call(actions, "focus_client", "up")
		end, {
			description = "Focus Client Up",
			group = "Client",
		}),

		awful.key({ modkey }, "Down", function()
			call(actions, "focus_client", "down")
		end, {
			description = "Focus Client Down",
			group = "Client",
		}),

		awful.key({ modkey, "Shift" }, "Right", function()
			call(actions, "swap_client", "right")
		end, {
			description = "Move Window Right",
			group = "Client",
		}),

		awful.key({ modkey, "Shift" }, "Left", function()
			call(actions, "swap_client", "left")
		end, {
			description = "Move Window Left",
			group = "Client",
		}),

		awful.key({ modkey, "Shift" }, "Up", function()
			call(actions, "swap_client", "up")
		end, {
			description = "Move Window Up",
			group = "Client",
		}),

		awful.key({ modkey, "Shift" }, "Down", function()
			call(actions, "swap_client", "down")
		end, {
			description = "Move Window Down",
			group = "Client",
		})
	)
end

return function(modkey, actions)
	return M.build(modkey, actions)
end
