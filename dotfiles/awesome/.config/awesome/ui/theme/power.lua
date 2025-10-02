-- ~/.config/awesome/ui/theme/power.lua
local Theme = {}

Theme.defaults = {
	-- Geometrie
	dialog_h = 360, -- << set explicitly
	dialog_radius = 0,
	dialog_border_width = 1,
	dialog_border = "#1A50B8",

	header_ratio = 0.22,
	footer_ratio = 0.22,
	header_h = 80,
	footer_h = 80,

	-- Flächen / Farben (Dialog)
	header_bg = "#1A50B8",
	header_fg = "#FFFFFF",
	body_bg = "#0B89E7",
	body_fg = "#000000",
	footer_bg = "#1A50B8",
	footer_fg = "#FFFFFF",

	-- Popup/Backdrop
	dialog_bg = "#053193",
	backdrop = "#00000066",

	-- Header Typo/Icon
	header_font_size = 18,
	header_icon = " XP",
	header_icon_size = 20,
	header_icon_path = "",

	-- Pads
	pad_h = 16,
	pad_v = 14,

	-- Icon/Action Cards
	icon_ratio = 0.22,
	icon_pad = 6,
	icon_cell_pad = 6,
	icon_cell_extra_w = 56,
	icon_spacing = 12,
	icon_label_size = 12,
	icon_label_leading = 1.25,
	icon_label_lines = 1,
	icon_label_color = "#FFFFFF",
	icon_shape = "rounded",
	icon_rounding = 10,
	icon_hover_bg = "#FFFFFF22",
	icon_hover_border = "#2B77FF",
	icon_hover_bw = 2,

	-- Footer / Cancel
	cancel_bg = "#F5F5EE",
	cancel_fg = "#000000",
	cancel_pad_h = 10,
	cancel_pad_v = 4,
	cancel_radius = 2,
	cancel_hover_bg = "#F5F5EE",
	cancel_hover_border = "#2B77FF",
	cancel_hover_bw = 2,
}

local function merge(a, b)
	local out = {}
	if type(a) == "table" then
		for k, v in pairs(a) do
			out[k] = v
		end
	end
	if type(b) == "table" then
		for k, v in pairs(b) do
			out[k] = v
		end
	end
	return out
end

function Theme.merge(a, b)
	return merge(a, b)
end

function Theme.get(overrides)
	return merge(Theme.defaults, overrides or {})
end

function Theme.init(_) end

return Theme
