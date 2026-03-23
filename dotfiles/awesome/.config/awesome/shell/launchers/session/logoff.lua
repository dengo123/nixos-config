-- ~/.config/awesome/shell/launchers/session/logoff.lua
local awful = require("awful")
local gears = require("gears")

local M = {}

local COMMANDS = {
	lock = { "dm-tool", "lock" },
	logout = { "pkill", "-KILL", "-u", os.getenv("USER") or "" },
	switch_user = { "dm-tool", "switch-to-greeter" },
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

local function logoff_cfg(cfg)
	cfg = cfg or {}

	local launchers_cfg = cfg.launchers or {}
	local session_cfg = launchers_cfg.session or {}

	return session_cfg.logoff or {}
end

local function show_switch_user(cfg)
	return logoff_cfg(cfg).show_switch_user == true
end

local function make_action(key, label, assets)
	local cmd = assert(COMMANDS[key], "session.logoff: unbekannter command key: " .. tostring(key))

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

function M.build(_th, cfg)
	local assets = {
		-- lock = "ui/assets/lock.png",
		-- logout = "ui/assets/logoff.png",
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
