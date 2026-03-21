-- ~/.config/awesome/input/global/awesome.lua
local awful = require("awful")

local M = {}

-- =========================================================================
-- Public API
-- =========================================================================

function M.build(modkey)
	return awful.util.table.join(
		awful.key({ modkey }, "r", awesome.restart, {
			description = "Reload Awesome",
			group = "Awesome",
		}),

		awful.key({ modkey, "Control" }, "r", function()
			awful.spawn.with_shell(
				"systemctl --user daemon-reexec && systemctl --user restart graphical-session.target"
			)
			awesome.restart()
		end, {
			description = "Reload Session",
			group = "Awesome",
		})
	)
end

return function(modkey)
	return M.build(modkey)
end
