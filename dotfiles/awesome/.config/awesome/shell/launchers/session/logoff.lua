-- ~/.config/awesome/shell/launchers/session/logoff.lua
local awful = require("awful")
local gears = require("gears")

local M = {}

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

local function logoff_cfg(cfg)
	cfg = cfg or {}

	local launchers_cfg = cfg.launchers or {}
	local session_cfg = launchers_cfg.session or {}

	return session_cfg.logoff or {}
end

local function show_switch_user(cfg)
	return logoff_cfg(cfg).show_switch_user == true
end

local function command_for(key)
	if key == "lock" then
		return { "dm-tool", "lock" }
	end

	if key == "switch_user" then
		return { "dm-tool", "switch-to-greeter" }
	end

	if key == "logout" then
		return { "pkill", "-KILL", "-u", os.getenv("USER") or "" }
	end

	error("session.logoff: unbekannter command key: " .. tostring(key))
end

local function make_action(key, label, assets)
	local cmd = command_for(key)

	return {
		key = key,
		icon = icon_from_assets(assets, key),
		label = label,
		on_press = function()
			awful.spawn(cmd, false)
		end,
	}
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.build(_, cfg)
	local assets = {
		-- lock = "ui/assets/Lock_Session.png",
		-- logout = "ui/assets/Log_Off.png",
		-- switch_user = "ui/assets/users.png",
	}

	local actions = {
		make_action("lock", "Lock Session", assets),
		make_action("logout", "Log Off", assets),
	}

	if show_switch_user(cfg) then
		table.insert(actions, make_action("switch_user", "Switch User", assets))
	end

	return {
		header_title = "Log Off Awesome",
		cancel_label = "Cancel",
		actions = actions,
	}
end

return M
