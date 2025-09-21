local gears = require("gears")
local wibox = require("wibox")
local theme = require("features.shell.menu.widgets.theme")

local M = {}

function M.build_header_content(user, t, avail_h)
	t = theme.with_defaults(t or {})
	user = user or {}

	local icon_sz = theme.resolve_icon_size(t, avail_h, "header")
	local font = theme.resolve_font(t, avail_h, "header")

	local avatar = wibox.widget({
		image = user.avatar or nil,
		resize = true,
		forced_width = icon_sz,
		forced_height = icon_sz,
		widget = wibox.widget.imagebox,
	})

	local name = wibox.widget({
		text = user.name or "",
		font = font,
		valign = "center",
		align = "left",
		widget = wibox.widget.textbox,
	})

	local sub = wibox.widget({
		text = user.subtitle or "",
		font = font,
		valign = "center",
		align = "left",
		widget = wibox.widget.textbox,
	})

	local has_sub = (user.subtitle and user.subtitle ~= "")
	sub.visible = has_sub

	local column = wibox.widget({
		name,
		sub,
		spacing = has_sub and (t.header_text_spacing or 2) or 0,
		layout = wibox.layout.fixed.vertical,
	})

	-- ✨ WICHTIG: Textspalte selbst vertikal mittig zum Avatar ausrichten
	local column_centered = wibox.widget({
		column,
		halign = "left",
		valign = "center",
		widget = wibox.container.place,
	})

	local line = wibox.widget({
		avatar,
		column_centered, -- <- statt 'column' direkt
		spacing = t.header_spacing or 10,
		layout = wibox.layout.fixed.horizontal,
	})

	-- optional: gesamte Zeile im gepaddeten Header mittig halten (kann bleiben)
	local placed = wibox.widget({ line, halign = "left", valign = "center", widget = wibox.container.place })

	local api = {}
	function api.set_user(name_txt, avatar_path, subtitle_txt)
		if name_txt ~= nil then
			name.text = name_txt
		end
		if avatar_path ~= nil then
			avatar.image = avatar_path
		end
		if subtitle_txt ~= nil then
			sub.text = subtitle_txt
			local show = (subtitle_txt ~= "")
			sub.visible = show
			column.spacing = show and (t.header_text_spacing or 2) or 0
		end
	end

	return { widget = placed, set_user = api.set_user }
end

return M
