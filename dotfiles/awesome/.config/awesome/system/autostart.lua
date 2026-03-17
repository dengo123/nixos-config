-- ~/.config/awesome/system/autostart.lua
local awful = require("awful")

local M = {}

local signals_ready = false
local tray_started = false

-- =========================================================================
-- Helpers
-- =========================================================================

local function shquote(s)
	s = tostring(s or "")
	return "'" .. s:gsub("'", "'\"'\"'") .. "'"
end

local function spawn_once(cmd, match_cmd)
	if not cmd or cmd == "" then
		return
	end

	local match = match_cmd or cmd

	awful.spawn.with_shell("pgrep -af " .. shquote(match) .. " >/dev/null 2>&1 || " .. cmd)
end

local function restart_later(cmd, kill_pattern, delay)
	delay = tonumber(delay) or 1.5

	awful.spawn.with_shell(
		"pkill -f "
			.. shquote(kill_pattern)
			.. " >/dev/null 2>&1; "
			.. "(sleep "
			.. tostring(delay)
			.. "; "
			.. cmd
			.. " >/dev/null 2>&1) &"
	)
end

local function start_tray_apps(cfg)
	if tray_started then
		return
	end

	tray_started = true

	local bar_cfg = cfg.bar or {}
	local tray_cfg = bar_cfg.systray or {}

	if tray_cfg.enable == false then
		return
	end

	if tray_cfg.startBlueman ~= false then
		spawn_once("blueman-applet", "[b]lueman-applet")
	end

	if tray_cfg.startPasystray ~= false then
		spawn_once("pasystray", "[p]asystray")
	end

	if tray_cfg.startNmApplet ~= false then
		spawn_once("nm-applet", "[n]m-applet")
	end

	if tray_cfg.startCopyQ ~= false then
		restart_later("copyq", "[c]opyq", tray_cfg.copyqDelay or 1.5)
	end
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(cfg)
	cfg = cfg or {}

	if signals_ready then
		return
	end

	signals_ready = true

	awesome.connect_signal("ui::tray_ready", function()
		start_tray_apps(cfg)
	end)
end

return M
