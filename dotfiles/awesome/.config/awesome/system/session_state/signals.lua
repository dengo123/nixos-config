-- ~/.config/awesome/system/session_state/signals.lua
local gears = require("gears")

local M = {}

local runtime = {
	api = {},
	cfg = {},
	ui = {},
	attached = false,
	save_timer = nil,
}

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

function M.init(args)
	args = args or {}
	runtime.api = args.api or {}
	runtime.cfg = args.cfg or {}
	runtime.ui = args.ui or {}
	return M
end

function M.attach(args)
	if runtime.attached then
		return
	end

	runtime.attached = true

	local snapshot = args and args.snapshot
	local restore = args and args.restore

	awesome.connect_signal("autorandr::pre", function()
		if type(snapshot) == "function" then
			snapshot()
		end
	end)

	awesome.connect_signal("autorandr::applied", function()
		if type(restore) == "function" then
			restore()
		end
	end)

	awesome.connect_signal("session::pre_change", function()
		if type(snapshot) == "function" then
			snapshot()
		end
	end)

	awesome.connect_signal("session::post_change", function()
		if type(restore) == "function" then
			restore()
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
end

return M
