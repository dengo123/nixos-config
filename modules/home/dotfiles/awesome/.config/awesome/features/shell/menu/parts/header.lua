-- ~/.config/awesome/features/shell/menu/parts/header.lua
local gears = require("gears")
local wibox = require("wibox")

local Header = {}

-- user: { name="...", avatar="path|image|surface", subtitle="optional" }
-- t: optional overrides (header_h, header_bg/fg, header_pad_*, header_spacing,
--    avatar_size, avatar_radius)
function Header.build(user, t)
	t = t or {}
	user = user or {}

	-- Maße/Farben (Defaults auf Luna-Blau + 64px)
	local H = t.header_h or 64
	local PAD_L = t.header_pad_l or 10
	local PAD_R = t.header_pad_r or 10
	local PAD_T = t.header_pad_t or 8
	local PAD_B = t.header_pad_b or 8
	local BG = t.header_bg or t.bg or "#3A6EA5" -- Luna-Blau (wie Footer)
	local FG = t.header_fg or t.fg or "#FFFFFF"

	-- Avatar
	local AV_SIZE = t.avatar_size or 40
	local AV_RAD = t.avatar_radius or 8
	local AV_SHAPE = function(cr, w, h)
		gears.shape.rounded_rect(cr, w, h, AV_RAD)
	end

	local avatar_img = wibox.widget({
		image = user.avatar or nil,
		resize = true,
		forced_width = AV_SIZE,
		forced_height = AV_SIZE,
		clip_shape = AV_SHAPE, -- runde Ecken
		widget = wibox.widget.imagebox,
	})

	-- Text
	local name_lbl = wibox.widget({
		text = user.name or "",
		valign = "center",
		widget = wibox.widget.textbox,
	})

	local subtitle_lbl = wibox.widget({
		text = user.subtitle or "",
		valign = "center",
		widget = wibox.widget.textbox,
	})

	local has_sub = (user.subtitle and user.subtitle ~= "")
	subtitle_lbl.visible = has_sub

	local text_column = wibox.widget({
		name_lbl,
		subtitle_lbl,
		spacing = has_sub and (t.header_text_spacing or 2) or 0,
		layout = wibox.layout.fixed.vertical,
	})

	-- Innenlayout
	local inner = wibox.widget({
		avatar_img,
		text_column,
		spacing = t.header_spacing or 10,
		layout = wibox.layout.fixed.horizontal,
	})

	-- Container
	local container = wibox.widget({
		{
			inner,
			left = PAD_L,
			right = PAD_R,
			top = PAD_T,
			bottom = PAD_B,
			widget = wibox.container.margin,
		},
		forced_height = H,
		bg = BG,
		fg = FG,
		widget = wibox.container.background,
	})

	local api = { widget = container }

	function api:set_user(name, avatar_path, subtitle)
		if name ~= nil then
			name_lbl.text = name
		end
		if avatar_path ~= nil then
			avatar_img.image = avatar_path
		end
		if subtitle ~= nil then
			subtitle_lbl.text = subtitle
			local show = (subtitle ~= "")
			subtitle_lbl.visible = show
			text_column.spacing = show and (t.header_text_spacing or 2) or 0
		end
	end

	return api
end

return Header
