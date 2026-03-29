-- ~/.config/awesome/system/session_state/signals.lua
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
		timeout = 0.75,
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

	awesome.connect_signal("autorandr::pre", function()
		if type(snapshot) == "function" then
			snapshot()
		end
	end)

	awesome.connect_signal("autorandr::applied", function()
		if type(restore) == "function" then
			restore({
				restore_screen = true,
				restore_tag = true,
				restore_state = true,
				restore_layout = true,
			})
		end
	end)

	awesome.connect_signal("session::pre_change", function()
		if type(snapshot) == "function" then
			snapshot()
		end
	end)

	awesome.connect_signal("session::post_change", function()
		if type(restore) == "function" then
			restore({
				restore_screen = false,
				restore_tag = false,
				restore_state = true,
				restore_layout = true,
			})
		end
	end)

	tag.connect_signal("property::selected", function()
		schedule_save(snapshot)
	end)

	tag.connect_signal("property::layout", function()
		schedule_save(snapshot)
	end)

	client.connect_signal("tagged", function()
		schedule_save(snapshot)
	end)

	client.connect_signal("untagged", function()
		schedule_save(snapshot)
	end)

	client.connect_signal("property::screen", function()
		schedule_save(snapshot)
	end)

	client.connect_signal("property::minimized", function()
		schedule_save(snapshot)
	end)

	client.connect_signal("property::fullscreen", function()
		schedule_save(snapshot)
	end)

	client.connect_signal("property::maximized", function()
		schedule_save(snapshot)
	end)

	client.connect_signal("property::floating", function()
		schedule_save(snapshot)
	end)

	client.connect_signal("manage", function()
		schedule_save(snapshot)
	end)

	client.connect_signal("unmanage", function()
		schedule_save(snapshot)
	end)

	screen.connect_signal("property::geometry", function()
		schedule_save(snapshot)
	end)

	return M
end

return M
