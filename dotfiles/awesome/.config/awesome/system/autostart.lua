-- ~/.config/awesome/system/autostart.lua
local awful = require("awful")

local M = {}

local signals_ready = false
local copyq_started = false

-- =========================================================================
-- Helpers
-- =========================================================================

local function shquote(s)
	s = tostring(s or "")
	return "'" .. s:gsub("'", "'\"'\"'") .. "'"
end

local function spawn_once_later(cmd, match_pattern, delay)
	delay = tonumber(delay) or 1.5

	awful.spawn.with_shell(
		"(sleep "
			.. tostring(delay)
			.. "; pgrep -af "
			.. shquote(match_pattern)
			.. " >/dev/null 2>&1 || "
			.. cmd
			.. ") &"
	)
end

local function start_copyq(cfg)
	if copyq_started then
		return
	end

	copyq_started = true

	local system_cfg = cfg.system or {}
	local autostart_cfg = system_cfg.autostart or {}
	local copyq_cfg = autostart_cfg.copyq or {}

	if copyq_cfg.enable == false then
		return
	end

	local delay = copyq_cfg.delay or 1.5

	spawn_once_later("copyq config showTray true >/dev/null 2>&1; copyq >/tmp/awesome-copyq.log 2>&1", "[c]opyq", delay)
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
		start_copyq(cfg)
	end)
end

return M
