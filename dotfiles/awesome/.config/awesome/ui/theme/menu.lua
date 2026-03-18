-- ~/.config/awesome/ui/theme/menu.lua
local beautiful = require("beautiful")
local gears = require("gears")
local xr = require("beautiful.xresources")

local dpi = xr.apply_dpi

local M = {}

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(cfg)
	cfg = cfg or {}

	-- ---------------------------------------------------------------------
	-- Colors
	-- ---------------------------------------------------------------------

	local C = cfg.colors or {}
	local H = cfg.helpers or {}

	local border = (H and H.adjust_color and C.white) and H.adjust_color(C.white, -12) or (C.black or "#000000")

	local bg_focus = C.white_focus or ((H and H.adjust_color and C.white) and H.adjust_color(C.white, -6)) or C.creme

	-- ---------------------------------------------------------------------
	-- Menu
	-- ---------------------------------------------------------------------

	beautiful.menu_bg_normal = C.white
	beautiful.menu_fg_normal = C.black
	beautiful.menu_bg_focus = bg_focus
	beautiful.menu_fg_focus = C.black
	beautiful.menu_border_color = border
	beautiful.menu_border_width = dpi(1)

	beautiful.menu_height = dpi(28)
	beautiful.menu_width = dpi(220)

	beautiful.menu_gap = dpi(4)
	beautiful.menu_x_padding = dpi(0)
	beautiful.menu_align = "left"
	beautiful.menu_x_offset = 0
	beautiful.menu_submenu = "›"

	beautiful.menu_shape = function(cr, w, h)
		gears.shape.rounded_rect(cr, w, h, beautiful.border_radius or dpi(6))
	end

	-- ---------------------------------------------------------------------
	-- Hotkeys
	-- ---------------------------------------------------------------------

	beautiful.hotkeys_bg = beautiful.menu_bg_normal
	beautiful.hotkeys_fg = beautiful.menu_fg_normal
	beautiful.hotkeys_border_width = beautiful.menu_border_width
	beautiful.hotkeys_border_color = beautiful.menu_border_color
	beautiful.hotkeys_shape = beautiful.menu_shape

	beautiful.hotkeys_label_bg = beautiful.menu_bg_focus
	beautiful.hotkeys_label_fg = beautiful.menu_fg_focus
	beautiful.hotkeys_modifiers_fg = beautiful.menu_fg_focus

	beautiful.hotkeys_font = beautiful.font or "Sans 10"
	beautiful.hotkeys_description_font = beautiful.font or "Sans 10"
	beautiful.hotkeys_group_margin = dpi(12)
end

function M.get()
	-- ---------------------------------------------------------------------
	-- Theme Object
	-- ---------------------------------------------------------------------

	return {
		width = beautiful.menu_width,
		height = beautiful.menu_height,
		bg_normal = beautiful.menu_bg_normal,
		fg_normal = beautiful.menu_fg_normal,
		bg_focus = beautiful.menu_bg_focus,
		fg_focus = beautiful.menu_fg_focus,
		border_color = beautiful.menu_border_color,
		border_width = beautiful.menu_border_width,
		shape = beautiful.menu_shape,
		submenu = beautiful.menu_submenu,
	}
end

function M.props()
	-- ---------------------------------------------------------------------
	-- Theme Object
	-- ---------------------------------------------------------------------

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
