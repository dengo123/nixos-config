-- windowing/container.lua
local gears = require("gears")
local beautiful = require("beautiful")

local C = {}
C.__index = C

function C.init(o)
	C.shape_fn = o and o.shape_fn or nil
end

function C.apply(self, c)
	if not (c and c.valid) then
		return
	end
	local maximized = c.fullscreen or c.maximized or c.maximized_vertical or c.maximized_horizontal

	local bw = beautiful.border_width or 0
	c.border_width = maximized and 0 or bw
	c.border_color = (c == client.focus) and (beautiful.border_focus or beautiful.border_normal)
		or beautiful.border_normal

	local fn = C.shape_fn
	if not fn then
		local r = tonumber(beautiful.border_radius) or 0
		if r > 0 then
			fn = function(cr, w, h)
				if (w or 0) >= 2 and (h or 0) >= 2 then
					gears.shape.rounded_rect(cr, w, h, r)
				end
			end
		end
	end
	c.shape = (not maximized and fn) or nil
end

return C
