-- features/shell/menu/primitives.lua
local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")

local P = {}

function P.row_widget(item, t)
	local icon_size = t.icon_size or 18
	local content = wibox.widget({
		{
			{
				{
					image = item.icon,
					resize = true,
					forced_height = icon_size,
					forced_width = icon_size,
					widget = wibox.widget.imagebox,
				},
				{
					text = item.text or "",
					widget = wibox.widget.textbox,
				},
				spacing = t.row_spacing or 8,
				layout = wibox.layout.fixed.horizontal,
			},
			left = t.row_pad_l or 10,
			right = t.row_pad_r or 10,
			top = t.row_pad_t or 4,
			bottom = t.row_pad_b or 4,
			widget = wibox.container.margin,
		},
		bg = t.row_bg or t.bg,
		fg = t.row_fg or t.fg,
		widget = wibox.container.background,
	})
	local hover_bg = t.row_bg_hover or t.bg_focus or t.row_bg or t.bg
	content:connect_signal("mouse::enter", function()
		content.bg = hover_bg
	end)
	content:connect_signal("mouse::leave", function()
		content.bg = t.row_bg or t.bg
	end)
	content:buttons(gears.table.join(awful.button({}, 1, function()
		if item.on_press then
			item.on_press()
		end
	end)))
	return content
end

function P.list_widget(items, t)
	local box = { layout = wibox.layout.fixed.vertical, spacing = t.list_spacing or 2 }
	for _, it in ipairs(items or {}) do
		table.insert(box, P.row_widget(it, t))
	end
	return wibox.widget(box)
end

function P.power_button(btn, t)
	local size = t.power_icon_size or 16
	local inner = wibox.widget({
		{
			image = btn.icon,
			resize = true,
			forced_height = size,
			forced_width = size,
			widget = wibox.widget.imagebox,
		},
		{
			text = btn.text or "",
			widget = wibox.widget.textbox,
		},
		spacing = t.power_spacing or 6,
		layout = wibox.layout.fixed.horizontal,
	})
	local box = wibox.widget({
		{
			inner,
			left = t.power_pad_l or 10,
			right = t.power_pad_r or 10,
			top = t.power_pad_t or 4,
			bottom = t.power_pad_b or 4,
			widget = wibox.container.margin,
		},
		bg = t.power_bg or t.footer_bg or t.bg,
		fg = t.power_fg or t.footer_fg or t.fg,
		shape = t.power_shape or t.shape or gears.shape.rectangle,
		widget = wibox.container.background,
	})
	local hover_bg = t.power_bg_hover or t.bg_focus or box.bg
	box:connect_signal("mouse::enter", function()
		box.bg = hover_bg
	end)
	box:connect_signal("mouse::leave", function()
		box.bg = t.power_bg or t.footer_bg or t.bg
	end)
	box:buttons(gears.table.join(awful.button({}, 1, function()
		if btn.on_press then
			btn.on_press()
		end
	end)))
	return box
end

return P
