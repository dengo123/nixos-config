-- ~/.config/awesome/input/client/kill.lua
local awful = require("awful")

local M = {}

-- =========================================================================
-- Helpers
-- =========================================================================

local function focused_client()
	return client.focus
end

local function window_id(c)
	if not c or not c.window then
		return nil
	end

	return tostring(c.window)
end

local function request_close_focused_client()
	local c = focused_client()
	if not c then
		return
	end

	local wid = window_id(c)
	if wid then
		awful.spawn({ "xdotool", "windowclose", wid }, false)
		return
	end

	c:kill()
end

local function kill_focused_client()
	local c = focused_client()
	if not c then
		return
	end

	c:kill()
end

local function sigkill_focused_client()
	local c = focused_client()
	if not c then
		return
	end

	local pid = tonumber(c.pid)
	if pid then
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
			request_close_focused_client()
		end, {
			description = "Request Close Focused Window",
			group = "Client",
		}),

		awful.key({ modkey, "Shift" }, "q", function()
			kill_focused_client()
		end, {
			description = "Kill Focused Window",
			group = "Client",
		}),

		awful.key({ "Mod1" }, "F4", function()
			sigkill_focused_client()
		end, {
			description = "SIGKILL Focused Window",
			group = "Client",
		})
	)
end

return function(modkey)
	return M.build(modkey)
end
