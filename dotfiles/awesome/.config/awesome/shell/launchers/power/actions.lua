-- ~/.config/awesome/shell/launchers/power/actions.lua
local awful = require("awful")
local gears = require("gears")

local M = {}

-- =========================================================================
-- Helpers
-- =========================================================================

local function icon_from_theme(th, key)
	local icons = th.icons or {}
	local path = icons[key]

	if type(path) ~= "string" or #path == 0 then
		return nil
	end

	if path:match("^/") then
		return path
	end

	return gears.filesystem.get_configuration_dir() .. path
end

local function make_power_action(cmd)
	return function()
		awful.spawn.with_shell(cmd)
	end
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.build(th)
	local labels = assert(th.labels, "power.actions: labels fehlen")

	return {
		{
			key = "hibernate",
			icon = icon_from_theme(th, "hibernate"),
			emoji = "🚪",
			label = labels.hibernate,
			on_press = make_power_action("systemctl hibernate"),
		},
		{
			key = "poweroff",
			icon = icon_from_theme(th, "poweroff"),
			emoji = "⏻",
			label = labels.poweroff,
			on_press = make_power_action("systemctl poweroff"),
		},
		{
			key = "reboot",
			icon = icon_from_theme(th, "reboot"),
			emoji = "🔄",
			label = labels.reboot,
			on_press = make_power_action("systemctl reboot"),
		},
	}
end

return M
