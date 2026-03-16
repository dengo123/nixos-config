-- ~/.config/awesome/shell/launchers/lib/actions.lua
local A = {}

-- ============================================================================
-- Helpers
-- ============================================================================

local function safe_close(ctrl, handle, gears)
	if ctrl and ctrl.cancel then
		ctrl.cancel()
	end

	if handle and handle.close then
		(gears and gears.timer or require("gears").timer).delayed_call(function()
			handle.close()
		end)
	end
end

-- ============================================================================
-- Public API
-- ============================================================================

function A.bind(ctx)
	ctx = ctx or {}

	local ctrl = ctx.ctrl
	local handle = ctx.handle
	local gears = ctx.gears

	local actions = {}

	actions["Cancel"] = function()
		safe_close(ctrl, handle, gears)
	end

	actions["OK"] = function()
		if ctrl and ctrl.submit then
			ctrl.submit()
		else
			safe_close(ctrl, handle, gears)
		end
	end

	actions["Mode"] = function()
		if ctrl and ctrl.rotate_mode then
			ctrl.rotate_mode()
		end
	end

	return actions
end

return A
