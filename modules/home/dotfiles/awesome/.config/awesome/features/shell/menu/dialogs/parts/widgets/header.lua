-- features/shell/menu/dialogs/parts/widgets/header.lua
local wibox = require("wibox")

local M = {}

local function pick(...)
	for i = 1, select("#", ...) do
		local v = select(i, ...)
		if v ~= nil then
			return v
		end
	end
end

-- Header-Inhalt (Icon + Titel) OHNE Container/Farbe/Margins
function M.mk_header_content(title, th)
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

	return wibox.widget({
		icon_widget,
		text_widget,
		spacing = pick(th.header_spacing, th.pad_h, 8),
		layout = wibox.layout.fixed.horizontal,
	})
end

return M
