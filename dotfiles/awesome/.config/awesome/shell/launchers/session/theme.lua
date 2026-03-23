local beautiful = require("beautiful")
local Colors = require("ui.colors")

local Theme = {}

-- ============================================================================
-- Helpers
-- ============================================================================

local function resolved_theme(args)
	local ui = (args and args.ui) or {}
	return ui.theme or {}
end

local function resolved_colors(args)
	local cfg = (args and args.cfg) or {}

	if type(Colors.set_runtime_cfg) == "function" then
		Colors.set_runtime_cfg(cfg)
	end

	return Colors.get()
end

local function with_size(font, size)
	assert(type(font) == "string" and font ~= "", "launchers.session.theme: font fehlt")
	assert(tonumber(size) ~= nil, "launchers.session.theme: size fehlt")

	local base = font:gsub("%s+%d+%.?%d*$", "")
	return base .. " " .. tostring(size)
end

-- ============================================================================
-- Theme
-- ============================================================================

function Theme.init(args)
	args = args or {}

	local theme = resolved_theme(args)
	local colors = resolved_colors(args)
	local fonts = theme.fonts or {}
	local icons = theme.icons or {}

	assert(
		type(colors.overlay_40) == "string" and colors.overlay_40 ~= "",
		"launchers.session.theme: colors.overlay_40 fehlt"
	)
	assert(
		type(colors.transparent) == "string" and colors.transparent ~= "",
		"launchers.session.theme: colors.transparent fehlt"
	)
	assert(
		type(colors.tertiary) == "string" and colors.tertiary ~= "",
		"launchers.session.theme: colors.tertiary fehlt"
	)
	assert(
		type(colors.secondary) == "string" and colors.secondary ~= "",
		"launchers.session.theme: colors.secondary fehlt"
	)
	assert(type(colors.white) == "string" and colors.white ~= "", "launchers.session.theme: colors.white fehlt")
	assert(type(colors.black) == "string" and colors.black ~= "", "launchers.session.theme: colors.black fehlt")

	assert(
		type(fonts.ui_bold) == "string" and fonts.ui_bold ~= "",
		"launchers.session.theme: theme.fonts.ui_bold fehlt"
	)
	assert(type(fonts.ui) == "string" and fonts.ui ~= "", "launchers.session.theme: theme.fonts.ui fehlt")

	assert(type(icons.system) == "string" and icons.system ~= "", "launchers.session.theme: theme.icons.system fehlt")

	beautiful.session = {
		dialog_w = 0,
		dialog_h = 360,
		dialog_radius = 0,
		dialog_border_width = 0,
		dialog_border = colors.tertiary,
		dialog_bg = colors.tertiary,
		backdrop = colors.overlay_40,

		header_ratio = 0.22,
		footer_ratio = 0.22,
		header_h = 80,
		footer_h = 80,

		header_bg = colors.tertiary,
		header_fg = colors.white,
		body_bg = colors.secondary,
		body_fg = colors.black,
		footer_bg = colors.tertiary,
		footer_fg = colors.white,

		header_font = with_size(fonts.ui_bold, 18),
		header_font_size = 18,
		header_icon_text = " XP",
		header_icon_path = icons.system,
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
		icon_label_color = colors.white,
		icon_label_pad_top = 0,
		icon_label_pad_bottom = 0,

		icon_shape = "rounded",
		icon_rounding = 14,

		icon_hover_bg = colors.white .. "22",
		icon_hover_border = colors.white,
		icon_hover_bw = 4,

		icon_focus_bg = colors.white .. "22",
		icon_focus_border = colors.white,
		icon_focus_bw = 4,

		transparent = colors.transparent,
	}
end

function Theme.get()
	return beautiful.session
end

return Theme
