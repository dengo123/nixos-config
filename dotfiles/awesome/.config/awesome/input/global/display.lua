-- ~/.config/awesome/input/global/display.lua
local awful = require("awful")

local M = {}

-- =========================================================================
-- Public API
-- =========================================================================

function M.build(modkey, _cfg)
	return awful.util.table.join(awful.key({ modkey }, "F5", function()
		awful.spawn("autorandr-toggle", false)
	end, {
		description = "Toggle Monitor Layout",
		group = "Display",
	}))
end

return function(modkey, cfg)
	return M.build(modkey, cfg)
end
