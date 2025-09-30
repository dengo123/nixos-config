-- shell/menu/widgets/header_content.lua
local wibox = require("wibox")
local Theme = require("shell.menu.lib.theme")

local M = {}

local function pick(...)
	for i = 1, select("#", ...) do
		local v = select(i, ...)
		if v ~= nil then
			return v
		end
	end
end

-- interne Helfer: links Widget, rechts optionaler Slot (z.B. Cancel)
local function build_line(left_widget, right_slot)
	-- Vertikal zentrieren nur für die linke Seite, horizontal KEIN Centering.
	local left_vcenter = wibox.widget({
		left_widget,
		halign = "left",
		valign = "center",
		widget = wibox.container.place,
	})

	return wibox.widget({
		left_vcenter, -- links bündig
		nil, -- optionaler Mittel-Bereich
		right_slot or nil, -- z.B. Cancel-Button rechts
		expand = "outside",
		layout = wibox.layout.align.horizontal,
	})
end

-- ========== Variante A: Dialog-Header (Icon + Title) ==========
-- Kompatibel zum alten mk_header_content(title, th)
function M.mk_header_content(title, th, right_slot)
	th = th or {}

	local icon_widget
	if th.header_icon_path and #th.header_icon_path > 0 then
		icon_widget = wibox.widget({
			image = th.header_icon_path,
			resize = true,
			forced_width = pick(th.header_icon_size, th.header_font_size, 20),
			forced_height = pick(th.header_icon_size, th.header_font_size, 20),
			widget = wibox.widget.imagebox,
		})
	else
		icon_widget = wibox.widget({
			markup = string.format(
				"<span font='sans %d'>%s</span>",
				pick(th.header_icon_size, th.header_font_size, 14),
				th.header_icon or ""
			),
			align = "center",
			valign = "center",
			widget = wibox.widget.textbox,
		})
	end

	local text_widget = wibox.widget({
		markup = string.format("<span font='sans %d'><b>%s</b></span>", pick(th.header_font_size, 14), title or ""),
		align = "left",
		valign = "center",
		widget = wibox.widget.textbox,
	})

	local left = wibox.widget({
		icon_widget,
		text_widget,
		spacing = pick(th.header_spacing, th.pad_h, 8),
		layout = wibox.layout.fixed.horizontal,
	})

	return build_line(left, right_slot)
end

-- ========== Variante B: User-Header (Avatar + Name + Subtitle) ==========
-- Dein bisheriges build_header_content – jetzt als build_user_header
-- Rückgabe: { widget = <wibox>, set_user = fn(name, avatar, subtitle) }
function M.build_user_header(user, th, avail_h, right_slot)
	th = Theme.with_defaults(th or {})
	user = user or {}

	local icon_sz = Theme.resolve_icon_size(th, avail_h, "header")
	local font = Theme.resolve_font(th, avail_h, "header")

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

	local col = wibox.widget({
		name,
		sub,
		spacing = has_sub and (th.header_text_spacing or 2) or 0,
		layout = wibox.layout.fixed.vertical,
	})

	local col_centered = wibox.widget({
		col,
		halign = "left",
		valign = "center",
		widget = wibox.container.place,
	})

	local left = wibox.widget({
		avatar,
		col_centered,
		spacing = th.header_spacing or 10,
		layout = wibox.layout.fixed.horizontal,
	})

	local widget = build_line(left, right_slot)

	local function set_user(name_txt, avatar_path, subtitle_txt)
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
			col.spacing = show and (th.header_text_spacing or 2) or 0
		end
	end

	return { widget = widget, set_user = set_user }
end

-- Back-compat alias: falls irgendwo noch build_header_content aufgerufen wird
M.build_header_content = function(user, th, avail_h, right_slot)
	return M.build_user_header(user, th, avail_h, right_slot)
end

return M
