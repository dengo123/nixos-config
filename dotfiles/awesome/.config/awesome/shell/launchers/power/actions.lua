-- ~/.config/awesome/shell/launchers/power/actions.lua
local awful = require("awful")
local gears = require("gears")

local M = {}

local COMMANDS = {
	hibernate = "systemctl hibernate",
	suspend = "systemctl suspend",
	poweroff = "systemctl poweroff",
	reboot = "systemctl reboot",
}

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

local function power_cfg(cfg)
	cfg = cfg or {}

	local launchers_cfg = cfg.launchers or {}
	local launcher_power_cfg = launchers_cfg.power or {}

	return launcher_power_cfg
end

local function resolve_first_action(cfg)
	local value = tostring(power_cfg(cfg).first_action or "hibernate"):lower()

	if value == "suspend" then
		return "suspend"
	end

	return "hibernate"
end

local function show_both_sleep_actions(cfg)
	return power_cfg(cfg).show_both_sleep_actions == true
end

local function make_action(th, key)
	local labels = assert(th.labels, "power.actions: labels fehlen")
	local cmd = assert(COMMANDS[key], "power.actions: unbekannter command key: " .. tostring(key))

	return {
		key = key,
		icon = icon_from_theme(th, key),
		label = labels[key],
		on_press = function()
			awful.spawn.with_shell(cmd)
		end,
	}
end

local function sleep_actions(th, cfg)
	local first = resolve_first_action(cfg)

	if show_both_sleep_actions(cfg) then
		if first == "suspend" then
			return {
				make_action(th, "suspend"),
				make_action(th, "hibernate"),
			}
		end

		return {
			make_action(th, "hibernate"),
			make_action(th, "suspend"),
		}
	end

	return {
		make_action(th, first),
	}
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.build(th, cfg)
	local actions = {}

	for _, action in ipairs(sleep_actions(th, cfg)) do
		table.insert(actions, action)
	end

	table.insert(actions, make_action(th, "poweroff"))
	table.insert(actions, make_action(th, "reboot"))

	return actions
end

return M
