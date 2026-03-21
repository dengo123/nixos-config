-- ~/.config/awesome/shell/launchers/lib/actions.lua
local A = {}

-- =========================================================================
-- Helpers
-- =========================================================================

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

-- =========================================================================
-- Public API
-- =========================================================================

function A.bind(ctx)
	ctx = ctx or {}

	local ctrl = ctx.ctrl
	local handle = ctx.handle
	local gears = ctx.gears

	return {
		cancel = function()
			safe_close(ctrl, handle, gears)
		end,

		ok = function()
			if ctrl and ctrl.submit then
				ctrl.submit()
			else
				safe_close(ctrl, handle, gears)
			end
		end,

		mode = function()
			if ctrl and ctrl.rotate_mode then
				ctrl.rotate_mode()
			end
		end,
	}
end

return A
