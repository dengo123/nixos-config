local awful = require("awful")

local M = {}

local grabber = nil
local active = false

local super_down = false
local used_with_other_key = false

-- =========================================================================
-- Internal
-- =========================================================================

local function reset_state()
	super_down = false
	used_with_other_key = false
end

local function any_overlay_open(overlays)
	local ordered = (overlays and overlays.ordered) or {}

	for _, overlay in ipairs(ordered) do
		local is_open = overlay.is_open

		if type(is_open) == "function" and is_open() then
			return true
		end
	end

	return false
end

local function should_fire(args)
	if any_overlay_open(args.overlays) then
		return false
	end

	return true
end

local function ensure_grabber(args)
	if grabber then
		return grabber
	end

	grabber = awful.keygrabber({
		stop_event = nil,

		keypressed_callback = function(_, _, key)
			if key == "Super_L" or key == "Super_R" then
				super_down = true
				used_with_other_key = false
				return
			end

			if super_down then
				used_with_other_key = true
			end
		end,

		keyreleased_callback = function(_, _, key)
			if key == "Super_L" or key == "Super_R" then
				local fire = super_down and not used_with_other_key and should_fire(args)
				reset_state()

				if fire then
					awesome.emit_signal("menu::toggle")
				end

				return
			end
		end,
	})

	return grabber
end

local function install(args)
	if active then
		return
	end

	active = true
	ensure_grabber(args):start()
end

local function uninstall()
	if not active then
		return
	end

	active = false
	reset_state()

	if grabber then
		grabber:stop()
	end
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(args)
	args = args or {}

	install(args)

	awesome.connect_signal("ui::overlays_changed", function()
		reset_state()
	end)
end

return M
