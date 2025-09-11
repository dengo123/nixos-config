-- features/shell/menu/header.lua
local gears = require("gears")
local wibox = require("wibox")

local Header = {}

-- user: { name="...", avatar="path|image|surface", subtitle="optional string" }
-- t: theme table (avatar_size, avatar_shape, header_* paddings/colors, header_spacing, etc.)
function Header.build(user, t)
	t = t or {}
	user = user or {}

	-- Avatar
	local avatar_img = wibox.widget({
		image = user.avatar or nil,
		resize = true,
		forced_width = t.avatar_size or 42,
		forced_height = t.avatar_size or 42,
		clip_shape = t.avatar_shape or gears.shape.rounded_rect,
		widget = wibox.widget.imagebox,
	})

	-- Textbereich (Name + optional Subtitle)
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

	-- Wenn kein subtitle gesetzt ist, blenden wir die Zeile „unsichtbar“ (height 0) aus
	local text_column = wibox.widget({
		name_lbl,
		subtitle_lbl,
		spacing = (user.subtitle and t.header_text_spacing) or 0,
		layout = wibox.layout.fixed.vertical,
	})
	if not user.subtitle or user.subtitle == "" then
		subtitle_lbl.visible = false
	end

	local inner = wibox.widget({
		avatar_img,
		text_column,
		spacing = t.header_spacing or 10,
		layout = wibox.layout.fixed.horizontal,
	})

	local container = wibox.widget({
		{
			inner,
			left = t.header_pad_l or 12,
			right = t.header_pad_r or 12,
			top = t.header_pad_t or 10,
			bottom = t.header_pad_b or 10,
			widget = wibox.container.margin,
		},
		bg = t.header_bg or t.bg,
		fg = t.header_fg or t.fg,
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
			subtitle_lbl.visible = (subtitle ~= "")
			text_column.spacing = (subtitle ~= "" and (t.header_text_spacing or 2)) or 0
		end
	end

	return api
end

return Header
