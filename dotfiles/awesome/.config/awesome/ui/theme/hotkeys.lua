-- ~/.config/awesome/ui/theme/hotkeys.lua
local beautiful = require("beautiful")
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

	local bg = beautiful.menu_bg_normal or C.white
	local fg = beautiful.menu_fg_normal or C.black
	local border_width = beautiful.menu_border_width or dpi(1)
	local border_color = beautiful.menu_border_color or C.black
	local shape = beautiful.menu_shape

	local label_bg = beautiful.menu_bg_focus or C.white_focus or C.creme
	local label_fg = beautiful.menu_fg_focus or C.black
	local modifiers_fg = beautiful.menu_fg_focus or C.black

	-- ---------------------------------------------------------------------
	-- Hotkeys
	-- ---------------------------------------------------------------------

	beautiful.hotkeys_bg = bg
	beautiful.hotkeys_fg = fg
	beautiful.hotkeys_border_width = border_width
	beautiful.hotkeys_border_color = border_color
	beautiful.hotkeys_shape = shape

	beautiful.hotkeys_label_bg = label_bg
	beautiful.hotkeys_label_fg = label_fg
	beautiful.hotkeys_modifiers_fg = modifiers_fg

	beautiful.hotkeys_font = beautiful.font or "Sans 10"
	beautiful.hotkeys_description_font = beautiful.font or "Sans 10"

	beautiful.hotkeys_group_margin = dpi(12)
end

return M
