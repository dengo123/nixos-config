-- ~/.config/awesome/input/global/dim.lua
local awful = require("awful")

local M = {}

-- =========================================================================
-- Public API
-- =========================================================================

function M.build(modkey, actions)
	actions = actions or {}

	return awful.util.table.join(awful.key({ modkey }, "d", function()
		if type(actions.screen_dim_toggle) == "function" then
			actions.screen_dim_toggle()
		end
	end, {
		description = "Toggle Screen Dim",
		group = "Global",
	}))
end

return function(modkey, actions)
	return M.build(modkey, actions)
end
