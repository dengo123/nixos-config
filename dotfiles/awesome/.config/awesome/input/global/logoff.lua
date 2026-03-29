-- ~/.config/awesome/input/global/logoff.lua
local awful = require("awful")
local gears = require("gears")

local M = {}

local function bind(mods, key, fn, desc)
	return awful.key(mods, key, fn, {
		description = desc,
		group = "Launchers",
	})
end

local function can_open_logoff(launchers_open)
	return launchers_open and type(launchers_open.logoff) == "function"
end

local function open_logoff_dialog(launchers_open)
	if not can_open_logoff(launchers_open) then
		return
	end

	launchers_open.logoff({})
end

function M.build(modkey, launchers_open)
	return gears.table.join(bind({ modkey }, "Pause", function()
		open_logoff_dialog(launchers_open)
	end, "Open Logoff Dialog"))
end

return function(modkey, launchers_open)
	return M.build(modkey, launchers_open)
end
