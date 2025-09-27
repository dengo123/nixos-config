local wibox = require("wibox")
local gears = require("gears")

local H = {}
local function pick(...)
	for i = 1, select("#", ...) do
		local v = select(i, ...)
		if v ~= nil then
			return v
		end
	end
end

function H.mk_header_content(title, th)
	th = th or {}
	local icon_w
	if th.header_icon_path and #th.header_icon_path > 0 then
		icon_w = wibox.widget({
			image = th.header_icon_path,
			resize = true,
			forced_width = pick(th.header_icon_size, th.header_font_size, 20),
			forced_height = pick(th.header_icon_size, th.header_font_size, 20),
			widget = wibox.widget.imagebox,
		})
	else
		icon_w = wibox.widget({
			markup = ("<span font='sans %d'>%s</span>"):format(
				pick(th.header_icon_size, th.header_font_size, 14),
				th.header_icon or ""
			),
			align = "center",
			valign = "center",
			widget = wibox.widget.textbox,
		})
	end
	local text_w = wibox.widget({
		markup = ("<span font='sans %d'><b>%s</b></span>"):format(pick(th.header_font_size, 14), title or ""),
		align = "left",
		valign = "center",
		widget = wibox.widget.textbox,
	})
	return wibox.widget({
		icon_w,
		text_w,
		spacing = pick(th.header_spacing, th.pad_h, 8),
		layout = wibox.layout.fixed.horizontal,
	})
end

return H
