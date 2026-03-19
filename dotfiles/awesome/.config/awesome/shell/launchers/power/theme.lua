-- ~/.config/awesome/shell/launchers/power/theme.lua
local beautiful = require("beautiful")

local Theme = {}

-- ============================================================================
-- Theme
-- ============================================================================

function Theme.init(cfg)
	cfg = cfg or {}

	local colors = cfg.colors or {}
	local backdrop = colors.overlay_40 or colors.black_40 or colors.transparent

	beautiful.power = {
		-- ---------------------------------------------------------------------
		-- Dialog
		-- ---------------------------------------------------------------------

		dialog_w = 0,
		dialog_h = 360,
		dialog_radius = 0,
		dialog_border_width = 0,
		dialog_border = colors.blue_dark,
		dialog_bg = colors.blue_dark,
		backdrop = backdrop,

		-- ---------------------------------------------------------------------
		-- Sections
		-- ---------------------------------------------------------------------

		header_ratio = 0.22,
		footer_ratio = 0.22,
		header_h = 80,
		footer_h = 80,

		header_bg = colors.blue_dark,
		header_fg = colors.white,
		body_bg = colors.blue_light,
		body_fg = colors.black,
		footer_bg = colors.blue_dark,
		footer_fg = colors.white,

		-- ---------------------------------------------------------------------
		-- Header
		-- ---------------------------------------------------------------------

		header_title = "Turn off Computer",
		header_font = "Sans",
		header_font_size = 18,
		header_icon_text = " XP",
		header_icon_path = "ui/assets/flake.png",
		header_icon_size = 56,
		header_pad_l = 18,
		header_pad_r = 18,
		header_pad_v = 0,

		-- ---------------------------------------------------------------------
		-- Body
		-- ---------------------------------------------------------------------

		pad_h = 24,
		pad_v = 24,

		-- ---------------------------------------------------------------------
		-- Actions
		-- ---------------------------------------------------------------------

		labels = {
			hibernate = "Sleep",
			suspend = "Stand By",
			poweroff = "Turn Off",
			reboot = "Restart",
		},

		cancel_label = "Cancel",

		-- ---------------------------------------------------------------------
		-- Icon Cards
		-- ---------------------------------------------------------------------

		icon_ratio = 0.33,
		icon_pad = 0,
		icon_cell_pad = 6,
		icon_cell_extra_w = 56,
		icon_spacing = 12,
		icon_label_size = 12,
		icon_label_leading = 1.25,
		icon_label_lines = 1,
		icon_label_color = colors.white,
		icon_label_pad_top = 0,
		icon_label_pad_bottom = 2,

		icon_shape = "rounded",
		icon_rounding = 14,

		icon_hover_bg = colors.white and (colors.white .. "22") or nil,
		icon_hover_border = colors.white,
		icon_hover_bw = 4,

		icon_focus_bg = colors.white and (colors.white .. "22") or nil,
		icon_focus_border = colors.white,
		icon_focus_bw = 4,

		-- ---------------------------------------------------------------------
		-- Assets
		-- ---------------------------------------------------------------------

		icons = {
			hibernate = "ui/assets/hibernate.png",
			suspend = "ui/assets/hibernate.png",
			poweroff = "ui/assets/poweroff.png",
			reboot = "ui/assets/reboot.png",
		},
	}
end

function Theme.get()
	return beautiful.power
end

return Theme
