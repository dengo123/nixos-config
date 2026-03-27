-- ~/.config/awesome/shell/notify/shape.lua
local gears = require("gears")

local M = {}

-- =========================================================================
-- Helpers
-- =========================================================================

local function rounded_shape(radius)
	return function(cr, w, h)
		gears.shape.rounded_rect(cr, w, h, radius)
	end
end

local function speech_shape(radius)
	return function(cr, w, h)
		local tail_w = 12
		local tail_h = 8
		local body_h = h - tail_h

		gears.shape.rounded_rect(cr, w, body_h, radius)

		cr:new_sub_path()
		cr:move_to(w - radius - tail_w, body_h)
		cr:line_to(w - radius - tail_w / 2, body_h + tail_h)
		cr:line_to(w - radius, body_h)
		cr:close_path()
	end
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.rounded(radius)
	return rounded_shape(radius)
end

function M.speech(radius)
	return speech_shape(radius)
end

return M
