-- ~/.config/awesome/ui/theme/menus.lua
local beautiful = require("beautiful")
local gears = require("gears")

local M = {}

function M.init(opts)
	opts = opts or {}
	-- Basisfarben (helles Beige, schwarze Schrift)
	beautiful.menu_bg_normal = opts.bg_normal or "#FFF7E6" -- warmes Weiß/Beige
	beautiful.menu_fg_normal = opts.fg_normal or "#000000"

	-- Fokuszustand leicht absetzen (etwas dunkleres Beige)
	beautiful.menu_bg_focus = opts.bg_focus or "#F2E7CF"
	beautiful.menu_fg_focus = opts.fg_focus or "#000000"

	-- Rahmen & Maße
	beautiful.menu_border_color = opts.border_color or "#E6D8BF"
	beautiful.menu_border_width = (opts.border_width ~= nil) and opts.border_width or 1
	beautiful.menu_height = opts.item_height or 28
	beautiful.menu_width = opts.width or 220

	-- Optionale Eckenrundung (falls gewünscht)
	beautiful.menu_shape = function(cr, w, h)
		gears.shape.rounded_rect(cr, w, h, opts.radius or (beautiful.border_radius or 6))
	end

	-- Optional: Submenü-Pfeil (Unicode) statt Icon
	beautiful.menu_submenu = opts.submenu_arrow or "›"
end

return M
