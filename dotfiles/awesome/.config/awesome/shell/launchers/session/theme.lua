-- ~/.config/awesome/shell/launchers/session/theme.lua
local beautiful = require("beautiful")
local gfs = require("gears.filesystem")

local Theme = {}

local runtime = {
	ui = {},
}

local DEFAULT_SYSTEM_ICON = gfs.get_configuration_dir() .. "ui/assets/flake.png"

-- ============================================================================
-- Helpers
-- ============================================================================

local function resolved_theme()
	local ui = runtime.ui or {}
	return ui.theme or {}
end

local function with_size(font, size)
	font = type(font) == "string" and font ~= "" and font or "Sans"
	size = tonumber(size) or 10

	local base = font:gsub("%s+%d+%.?%d*$", "")
	return base .. " " .. tostring(size)
end

-- ============================================================================
-- Theme
-- ============================================================================

function Theme.init(opts)
	opts = opts or {}
	runtime.ui = opts.ui or runtime.ui or {}

	local theme = resolved_theme()
	local colors = theme.colors or {}
	local fonts = theme.fonts or {}
	local icons = theme.icons or {}
	local utils = theme.utils or {}

	local system_icon = DEFAULT_SYSTEM_ICON
	if type(icons.system) == "string" and icons.system ~= "" then
		system_icon = icons.system
	end

	beautiful.session = {
		dialog_w = 0,
		dialog_h = 360,
		dialog_radius = 0,
		dialog_border_width = 0,
		dialog_border = colors.tertiary,
		dialog_bg = colors.tertiary,
		backdrop = utils.overlay_40,

		header_ratio = 0.22,
		footer_ratio = 0.22,
		header_h = 80,
		footer_h = 80,

		header_bg = colors.tertiary,
		header_fg = colors.text_invert or colors.background,
		body_bg = colors.secondary,
		body_fg = colors.text or colors.foreground,
		footer_bg = colors.tertiary,
		footer_fg = colors.text_invert or colors.background,

		header_font = with_size(fonts.ui_bold, 18),
		header_font_size = 18,
		header_icon_text = " XP",
		header_icon_path = system_icon,
		header_icon_size = 56,
		header_pad_l = 18,
		header_pad_r = 18,
		header_pad_v = 0,

		pad_h = 24,
		pad_v = 24,

		cancel_label = "Cancel",

		icon_ratio = 0.33,
		icon_pad = 0,
		icon_cell_pad = 6,
		icon_cell_extra_w = 56,
		icon_spacing = 12,
		icon_label_font = with_size(fonts.ui, 12),
		icon_label_size = 12,
		icon_label_leading = 1.25,
		icon_label_lines = 1,
		icon_label_color = colors.text_invert or colors.background,
		icon_label_pad_top = 0,
		icon_label_pad_bottom = 0,

		icon_shape = "rounded",
		icon_rounding = 14,

		icon_hover_bg = (colors.background or "") .. "22",
		icon_hover_border = colors.background,
		icon_hover_bw = 4,

		icon_focus_bg = (colors.background or "") .. "22",
		icon_focus_border = colors.background,
		icon_focus_bw = 4,

		transparent = utils.transparent,
	}

	return Theme
end

function Theme.get()
	return beautiful.session
end

return Theme
