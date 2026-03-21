local awful = require("awful")

local M = {}

-- =========================================================================
-- Helpers
-- =========================================================================

local function focused_client()
	return client.focus
end

local function close_focused_client()
	local c = focused_client()
	if c then
		c:kill()
	end
end

local function hard_kill_focused_client()
	local c = focused_client()
	if not c then
		return
	end

	local pid = c.pid

	if pid and tonumber(pid) then
		awful.spawn({ "kill", "-KILL", tostring(pid) }, false)
		return
	end

	c:kill()
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.build(modkey)
	return awful.util.table.join(
		awful.key({ modkey }, "q", function()
			close_focused_client()
		end, {
			description = "Close Focused Window",
			group = "Client",
		}),

		awful.key({ modkey, "Shift" }, "q", function()
			hard_kill_focused_client()
		end, {
			description = "Force Kill Focused Window",
			group = "Client",
		})
	)
end

return function(modkey)
	return M.build(modkey)
end
