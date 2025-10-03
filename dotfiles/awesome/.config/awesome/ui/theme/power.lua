-- ~/.config/awesome/ui/theme/power.lua
local beautiful = require("beautiful")

local Theme = {}

-- Quelle der Wahrheit für Power-Dialoge
function Theme.init(cfg)
	cfg = cfg or {}
	local C = cfg.colors or {}

	-- Tipp: Lege in colors.lua optional einen halbtransparenten Schwarzton an,
	-- z.B. overlay_40 = "#00000066". Falls nicht vorhanden, nimm transparent.
	local backdrop = C.overlay_40 or C.black_40 or C.transparent

	beautiful.power = {
		-- Geometrie
		dialog_w = 0, -- 0 = vom Layout berechnet
		dialog_h = 360,
		dialog_radius = 0,
		dialog_border_width = 2,
		dialog_border = C.blue_dark,

		-- Optional: Ratios (werden nur genutzt, wenn header_h/footer_h nil sind)
		header_ratio = 0.22,
		footer_ratio = 0.22,
		header_h = 80,
		footer_h = 80,

		-- Flächen/Farben
		dialog_bg = C.blue_dark, -- Rahmen + Header/Footer = blue_dark
		backdrop = backdrop, -- z.B. "#00000066" in colors.lua als overlay_40
		header_bg = C.blue_dark,
		header_fg = C.white,
		body_bg = C.blue_light, -- Body = blue_light
		body_fg = C.black,
		footer_bg = C.blue_dark,
		footer_fg = C.white,

		-- Header-Content
		header_title = "Turn off Computer",
		header_font_size = 18,
		header_icon_text = " XP",
		header_icon_path = "ui/assets/flake.png", -- Pfad hat Vorrang vor Text
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
		icon_label_color = C.white,
		icon_shape = "rounded",
		icon_rounding = 14,
		icon_hover_bg = C.white and (C.white .. "22") or nil, -- optional, sonst im View setzen
		icon_hover_border = C.white,
		icon_hover_bw = 4,

		icons = {
			hibernate = "ui/assets/hibernate.png",
			poweroff = "ui/assets/poweroff.png",
			reboot = "ui/assets/reboot.png",
		},
	}
end

function Theme.get()
	return beautiful.power
end

return Theme
