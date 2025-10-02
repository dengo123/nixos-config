-- ~/.config/awesome/ui/theme/menu.lua
local beautiful = require("beautiful")
local gears = require("gears")

local M = {}

-- Einfache, feste Vorgaben (keine Overrides per opts)
function M.init()
	-- Farben
	beautiful.menu_bg_normal = "#FFF7E6"
	beautiful.menu_fg_normal = "#000000"
	beautiful.menu_bg_focus = "#F2E7CF"
	beautiful.menu_fg_focus = "#000000"

	-- Maße
	beautiful.menu_border_color = "#E6D8BF"
	beautiful.menu_border_width = 1
	beautiful.menu_height = 28
	beautiful.menu_width = 220

	-- Shape
	beautiful.menu_shape = function(cr, w, h)
		gears.shape.rounded_rect(cr, w, h, beautiful.border_radius or 6)
	end
	beautiful.menu_submenu = "›"

	-- Placement/Rules (global gültig für Start & Tabs)
	beautiful.menu_gap = 4 -- Abstand zur Bar
	beautiful.menu_x_padding = 0 -- linker Innenabstand ab Bar/Start
	beautiful.menu_align = "left"
	beautiful.menu_x_offset = 0 -- <<< globaler zusätzlicher X-Offset (negativ = nach links)
end

function M.props()
	return {
		item_height = beautiful.menu_height,
		width = beautiful.menu_width,
		gap = beautiful.menu_gap,
		x_padding = beautiful.menu_x_padding,
		align = beautiful.menu_align,
		x_offset = beautiful.menu_x_offset,
	}
end

return M
