-- ~/.config/awesome/shell/windowing/container.lua
local gears = require("gears")
local beautiful = require("beautiful")

local C = {}

-- =========================================================================
-- Public API
-- =========================================================================

function C.init(o)
	o = o or {}

	-- ---------------------------------------------------------------------
	-- Config
	-- ---------------------------------------------------------------------

	C.shape_fn = o.shape_fn or nil
end

function C.apply(c)
	-- ---------------------------------------------------------------------
	-- Guard
	-- ---------------------------------------------------------------------

	if not (c and c.valid) then
		return
	end

	-- ---------------------------------------------------------------------
	-- State
	-- ---------------------------------------------------------------------

	local maximized = c.fullscreen or c.maximized or c.maximized_vertical or c.maximized_horizontal

	-- ---------------------------------------------------------------------
	-- Border
	-- ---------------------------------------------------------------------

	local border_width =
		assert(tonumber(beautiful.border_width), "windowing.container: beautiful.border_width fehlt/ungültig")
	local border_normal = assert(beautiful.border_normal, "windowing.container: beautiful.border_normal fehlt")
	local border_focus = assert(beautiful.border_focus, "windowing.container: beautiful.border_focus fehlt")

	c.border_width = maximized and 0 or border_width
	c.border_color = (c == client.focus) and border_focus or border_normal

	-- ---------------------------------------------------------------------
	-- Shape
	-- ---------------------------------------------------------------------

	local shape_fn = C.shape_fn

	if not shape_fn then
		local radius =
			assert(tonumber(beautiful.border_radius), "windowing.container: beautiful.border_radius fehlt/ungültig")

		if radius > 0 then
			shape_fn = function(cr, w, h)
				if (w or 0) >= 2 and (h or 0) >= 2 then
					gears.shape.rounded_rect(cr, w, h, radius)
				end
			end
		end
	end

	c.shape = (not maximized and shape_fn) or nil
end

return C
