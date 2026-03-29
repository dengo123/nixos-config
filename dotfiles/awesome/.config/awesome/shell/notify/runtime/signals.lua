-- ~/.config/awesome/shell/notify/runtime/signals.lua
local gears = require("gears")

local M = {}

local runtime = {
	attached = false,
	save_timer = nil,
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function schedule_save(snapshot_fn)
	if runtime.save_timer then
		runtime.save_timer:again()
		return
	end

	runtime.save_timer = gears.timer({
		timeout = 0.50,
		autostart = true,
		single_shot = true,
		callback = function()
			runtime.save_timer = nil
			if type(snapshot_fn) == "function" then
				snapshot_fn()
			end
		end,
	})
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(_)
	return M
end

function M.attach(args)
	args = args or {}

	if runtime.attached then
		return M
	end

	runtime.attached = true

	local snapshot = args.snapshot
	local restore = args.restore

	awesome.connect_signal("notify::history_changed", function()
		schedule_save(snapshot)
	end)

	awesome.connect_signal("session::post_change", function()
		if type(restore) == "function" then
			restore()
		end
	end)

	awesome.connect_signal("autorandr::applied", function()
		if type(restore) == "function" then
			restore()
		end
	end)

	return M
end

return M
