-- ~/.config/awesome/shell/launchers/session/power.lua
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

local function icon_from_assets(assets, key)
	local path = assets and assets[key]

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
	local session_cfg = launchers_cfg.session or {}

	return session_cfg.power or {}
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

local function make_action(key, label, assets)
	local cmd = assert(COMMANDS[key], "session.power: unbekannter command key: " .. tostring(key))

	return {
		key = key,
		icon = icon_from_assets(assets, key),
		label = label,
		on_press = function()
			awful.spawn.with_shell(cmd)
		end,
	}
end

local function sleep_actions(cfg, assets)
	local first = resolve_first_action(cfg)

	if show_both_sleep_actions(cfg) then
		if first == "suspend" then
			return {
				make_action("suspend", "Stand By", assets),
				make_action("hibernate", "Sleep", assets),
			}
		end

		return {
			make_action("hibernate", "Sleep", assets),
			make_action("suspend", "Stand By", assets),
		}
	end

	if first == "suspend" then
		return {
			make_action("suspend", "Stand By", assets),
		}
	end

	return {
		make_action("hibernate", "Sleep", assets),
	}
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.build(_, cfg)
	local assets = {
		hibernate = "ui/assets/hibernate.png",
		suspend = "ui/assets/hibernate.png",
		poweroff = "ui/assets/poweroff.png",
		reboot = "ui/assets/reboot.png",
	}

	local actions = {}

	for _, action in ipairs(sleep_actions(cfg, assets)) do
		table.insert(actions, action)
	end

	table.insert(actions, make_action("poweroff", "Turn Off", assets))
	table.insert(actions, make_action("reboot", "Restart", assets))

	return {
		header_title = "Turn off Computer",
		cancel_label = "Cancel",
		actions = actions,
	}
end

return M
