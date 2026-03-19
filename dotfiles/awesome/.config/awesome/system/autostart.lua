-- ~/.config/awesome/system/autostart.lua
local awful = require("awful")

local M = {}

local started = false

-- =========================================================================
-- Helpers
-- =========================================================================

local function shquote(s)
	s = tostring(s or "")
	return "'" .. s:gsub("'", "'\"'\"'") .. "'"
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

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(cfg)
	cfg = cfg or {}

	if started then
		return
	end

	started = true

	local system_cfg = cfg.system or {}
	local autostart_cfg = system_cfg.autostart or {}
	local copyq_cfg = autostart_cfg.copyq or {}

	if copyq_cfg.enable == false then
		return
	end

	restart_later(
		"copyq config showTray true >/dev/null 2>&1; copyq >/tmp/awesome-copyq.log 2>&1",
		"copyq",
		copyq_cfg.delay or 1.5
	)
end

return M
