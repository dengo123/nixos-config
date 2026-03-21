-- ~/.config/awesome/ui/wallpaper/rotation.lua
local gears = require("gears")

local M = {}

local runtime_cfg = {}
local refresh_fn = nil
local spec_fn = nil
local source_api = nil

local timers_by_screen = {}

-- =========================================================================
-- Helpers
-- =========================================================================

local function screen_key(s)
	return tostring((s and s.index) or 0)
end

local function stop_timer_for_screen(s)
	local key = screen_key(s)
	local timer = timers_by_screen[key]

	if timer then
		timer:stop()
		timers_by_screen[key] = nil
	end
end

local function rotation_enabled(spec)
	local rotation = (spec and spec.rotation) or {}
	return rotation.enabled == true
end

local function rotation_interval(spec)
	local rotation = (spec and spec.rotation) or {}
	local interval = tonumber(rotation.interval)

	if not interval or interval <= 0 then
		return 600
	end

	return interval
end

local function can_rotate(spec)
	if not rotation_enabled(spec) then
		return false
	end

	if not source_api or type(source_api.advance_for_screen) ~= "function" then
		return false
	end

	return true
end

local function start_timer_for_screen(s, spec)
	local key = screen_key(s)

	stop_timer_for_screen(s)

	timers_by_screen[key] = gears.timer({
		timeout = rotation_interval(spec),
		autostart = true,
		call_now = false,
		single_shot = false,
		callback = function()
			local current_spec = type(spec_fn) == "function" and spec_fn(s) or spec

			if not can_rotate(current_spec) then
				stop_timer_for_screen(s)
				return
			end

			source_api.advance_for_screen(s, current_spec)

			if type(refresh_fn) == "function" then
				refresh_fn()
			end
		end,
	})
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.set_runtime_cfg(cfg)
	runtime_cfg = cfg or {}
end

function M.init(args)
	args = args or {}

	runtime_cfg = args.cfg or {}
	refresh_fn = args.refresh
	spec_fn = args.spec_for_screen
	source_api = args.source_api
end

function M.sync_screen(s, spec)
	if not s then
		return
	end

	if can_rotate(spec) then
		start_timer_for_screen(s, spec)
		return
	end

	stop_timer_for_screen(s)
end

function M.sync_all(spec_resolver)
	for s in screen do
		local spec = type(spec_resolver) == "function" and spec_resolver(s) or nil
		M.sync_screen(s, spec)
	end
end

function M.stop_screen(s)
	stop_timer_for_screen(s)
end

function M.stop_all()
	for key, timer in pairs(timers_by_screen) do
		if timer then
			timer:stop()
		end
		timers_by_screen[key] = nil
	end
end

return M
