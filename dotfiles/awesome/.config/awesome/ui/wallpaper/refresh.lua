-- ~/.config/awesome/ui/wallpaper/refresh.lua
local gears = require("gears")

local M = {}

local runtime_cfg = {}
local geometry_handler = nil
local refresh_fn = nil

local debounce = gears.timer({
	timeout = 0.12,
	autostart = false,
	single_shot = true,
	callback = function()
		if type(refresh_fn) == "function" then
			refresh_fn()
		end

		gears.timer.start_new(0.35, function()
			if type(refresh_fn) == "function" then
				refresh_fn()
			end
			return false
		end)
	end,
})

-- =========================================================================
-- Helpers
-- =========================================================================

local function schedule_refresh()
	if debounce.started then
		debounce:stop()
	end

	debounce:start()
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

	if geometry_handler then
		screen.disconnect_signal("property::geometry", geometry_handler)
	end

	geometry_handler = function()
		schedule_refresh()
	end

	screen.connect_signal("property::geometry", geometry_handler)

	awesome.connect_signal("ui::wallpaper_refresh", function()
		schedule_refresh()
	end)

	awesome.connect_signal("autorandr::applied", function()
		schedule_refresh()
	end)
end

function M.schedule()
	schedule_refresh()
end

return M
