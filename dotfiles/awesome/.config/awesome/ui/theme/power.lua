-- ~/.config/awesome/ui/theme/power.lua
local Theme = {}

-- Einzige Quelle der Wahrheit für Power-Dialoge
Theme.defaults = {
	-- Geometrie
	dialog_w = 0, -- 0 = vom Layout berechnet, sonst px
	dialog_h = 360,
	dialog_radius = 8,
	dialog_border_width = 2,
	dialog_border = "#1A50B8",

	-- Optional: statt fixer Höhen können Ratios greifen
	header_ratio = 0.22, -- nur genutzt, wenn header_h/footer_h nicht gesetzt
	footer_ratio = 0.22,
	header_h = 80,
	footer_h = 80,

	-- Flächen/Farben
	dialog_bg = "#053193",
	backdrop = "#00000066",

	header_bg = "#1A50B8",
	header_fg = "#FFFFFF",
	body_bg = "#0B89E7",
	body_fg = "#000000",
	footer_bg = "#1A50B8",
	footer_fg = "#FFFFFF",

	-- Header-Content
	header_title = "Turn off Computer",
	header_font_size = 18,
	header_icon_text = " XP", -- alternativ:
	header_icon_path = "ui/assets/flake.png", -- falls Pfad gesetzt, hat Vorrang vor Text
	header_icon_size = 56,
	header_pad_l = 18,
	header_pad_r = 18,

	-- Innenabstände
	pad_h = 24,
	pad_v = 24,

	-- Icon/Action Cards
	icon_ratio = 0.33,
	icon_pad = 0,
	icon_cell_pad = 6,
	icon_cell_extra_w = 56,
	icon_spacing = 12,
	icon_label_size = 12,
	icon_label_leading = 1.25,
	icon_label_lines = 1,
	icon_label_color = "#FFFFFF",
	icon_shape = "rounded",
	icon_rounding = 14,
	icon_hover_bg = "#FFFFFF22",
	icon_hover_border = "#FFFFFF",
	icon_hover_bw = 4,
	icons = {
		hibernate = "ui/assets/hibernate.png",
		poweroff = "ui/assets/poweroff.png",
		reboot = "ui/assets/reboot.png",
	},
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

function Theme.get(overrides)
	return merge(Theme.defaults, overrides or {})
end

return Theme
