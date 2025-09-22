-- ~/.config/awesome/features/shell/menu/dialogs/hotkeys/theme.lua
local T = {}

local DEFAULTS = {
	-- Geometrie
	dialog_w = 900,
	dialog_h = 560,

	-- Farben (Hotkeys-eigen)
	header_bg = "#1D4ED8", -- blau
	header_fg = "#FFFFFF",
	body_bg = "#0B1020",
	body_fg = "#E5EAF5",

	-- Rahmen/Rundung/Backdrop
	dialog_radius = 8,
	dialog_border_width = 2,
	dialog_border = "#1D4ED8",
	dialog_backdrop = "#00000088",

	-- Abstände/Typo
	pad_h = 12,
	pad_v = 10,
	header_pad_h = 12,
	header_pad_v = 6,
	header_font_size = 12,
	footer_pad_v = 8,
	footer_btn_spacing = 8,

	-- Buttons (Close, etc.)
	cancel_pad_h = 10,
	cancel_pad_v = 4,
	cancel_radius = 4,
	btn_bg = "#334155",
	btn_fg = "#E5EAF5",
}

local function shallow_copy(a)
	local o = {}
	for k, v in pairs(a or {}) do
		o[k] = v
	end
	return o
end

function T.merge(a, b)
	local out = shallow_copy(a or {})
	for k, v in pairs(b or {}) do
		out[k] = v
	end
	return out
end

-- opts kann Overrides enthalten (z.B. von Columns)
function T.get(opts)
	return T.merge(DEFAULTS, opts or {})
end

return T
