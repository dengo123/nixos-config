-- ~/.config/awesome/shell/menu/theme.lua
local beautiful = require("beautiful")
local gears = require("gears")
local xr = require("beautiful.xresources")

local dpi = xr.apply_dpi

local M = {}

-- =========================================================================
-- Helpers
-- =========================================================================

local function resolved_theme(args)
	local ui = (args and args.ui) or {}
	return ui.theme or {}
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(args)
	args = args or {}

	local theme = resolved_theme(args)
	local C = theme.colors or {}
	local F = theme.fonts or {}

	-- ---------------------------------------------------------------------
	-- Menu
	-- ---------------------------------------------------------------------

	beautiful.menu_bg_normal = C.surface
	beautiful.menu_fg_normal = C.text or C.foreground
	beautiful.menu_bg_focus = C.surface_focus
	beautiful.menu_fg_focus = C.text or C.foreground
	beautiful.menu_border_color = C.surface_focus
	beautiful.menu_border_width = dpi(1)

	beautiful.menu_height = dpi(28)
	beautiful.menu_width = dpi(220)

	beautiful.menu_shape = function(cr, w, h)
		gears.shape.rounded_rect(cr, w, h, beautiful.border_radius or dpi(6))
	end

	beautiful.menu_submenu = "›"

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

	beautiful.hotkeys_font = F.ui
	beautiful.hotkeys_description_font = F.ui
	beautiful.hotkeys_group_margin = dpi(12)
end

function M.get()
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

return M
