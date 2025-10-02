-- ~/.config/awesome/ui/theme/menu.lua
local beautiful = require("beautiful")
local gears = require("gears")

local M = {}

function M.init()
	-- Menu-Farben/-Maße/-Shape
	beautiful.menu_bg_normal = "#FFF7E6"
	beautiful.menu_fg_normal = "#000000"
	beautiful.menu_bg_focus = "#F2E7CF"
	beautiful.menu_fg_focus = "#000000"
	beautiful.menu_border_color = "#E6D8BF"
	beautiful.menu_border_width = 1
	beautiful.menu_height = 28
	beautiful.menu_width = 220
	beautiful.menu_shape = function(cr, w, h)
		gears.shape.rounded_rect(cr, w, h, beautiful.border_radius or 6)
	end
	beautiful.menu_submenu = "›"
	beautiful.menu_gap = 4
	beautiful.menu_x_padding = 0
	beautiful.menu_align = "left"
	beautiful.menu_x_offset = 0

	---------------------------------------------------------------------------
	-- Hotkeys-Popup an Menü-Theme anlehnen
	---------------------------------------------------------------------------
	beautiful.hotkeys_bg = beautiful.menu_bg_normal
	beautiful.hotkeys_fg = beautiful.menu_fg_normal
	beautiful.hotkeys_border_width = beautiful.menu_border_width
	beautiful.hotkeys_border_color = beautiful.menu_border_color
	beautiful.hotkeys_shape = beautiful.menu_shape

	-- Labels/Highlights im Popup
	beautiful.hotkeys_label_bg = beautiful.menu_bg_focus
	beautiful.hotkeys_label_fg = beautiful.menu_fg_focus
	beautiful.hotkeys_modifiers_fg = beautiful.menu_fg_focus -- z.B. für "Ctrl", "Alt", …

	-- Typografie (optional — falls du nichts setzt, nutzt Awesome Defaults)
	beautiful.hotkeys_font = beautiful.font or "Sans 10"
	beautiful.hotkeys_description_font = beautiful.font or "Sans 10"

	-- Layout/Abstände (optional)
	beautiful.hotkeys_group_margin = 12
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
