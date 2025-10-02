-- ~/.config/awesome/shell/launchers/power/container.lua
local wibox = require("wibox")
local gears = require("gears")

local M = {}

local function pick(...)
	for i = 1, select("#", ...) do
		local v = select(i, ...)
		if v ~= nil then
			return v
		end
	end
end

-- build(th, dims, { title, body, cancel_btn })
function M.build(th, dims, slots)
	local title = slots.title or "Turn off Computer"
	local body_core = slots.body
	local cancel_btn = slots.cancel_btn

	local header = wibox.widget({
		{
			{ markup = ("<b>%s</b>"):format(title), widget = wibox.widget.textbox },
			left = dims.pad_h,
			right = dims.pad_h,
			widget = wibox.container.margin,
		},
		bg = pick(th.header_bg, "#235CDB"),
		fg = pick(th.header_fg, "#FFFFFF"),
		widget = wibox.container.background,
	})

	local body = wibox.widget({
		{
			{ body_core, halign = "center", valign = "center", widget = wibox.container.place },
			left = dims.pad_h,
			right = dims.pad_h,
			top = dims.pad_v,
			bottom = dims.pad_v,
			widget = wibox.container.margin,
		},
		bg = pick(th.body_bg, "#00000000"),
		fg = pick(th.body_fg, "#FFFFFF"),
		widget = wibox.container.background,
	})

	local footer = wibox.widget({
		{
			{ cancel_btn, halign = "right", valign = "center", widget = wibox.container.place },
			left = dims.pad_h,
			right = dims.pad_h,
			top = dims.pad_v,
			bottom = dims.pad_v,
			widget = wibox.container.margin,
		},
		bg = pick(th.body_bg, "#00000000"), -- Footer = Body-Farbe
		fg = pick(th.body_fg, "#FFFFFF"),
		widget = wibox.container.background,
	})

	local stack = wibox.widget({
		{ header, strategy = "exact", height = dims.header_h, widget = wibox.container.constraint },
		{ body, strategy = "exact", height = dims.body_h, widget = wibox.container.constraint },
		{ footer, strategy = "exact", height = dims.footer_h, widget = wibox.container.constraint },
		layout = wibox.layout.fixed.vertical,
	})

	local radius = tonumber(th.dialog_radius) or 8
	local border_w = tonumber(th.dialog_border_width) or 2
	local border_c = th.dialog_border or "#FFFFFF"

	return wibox.widget({
		stack,
		shape = function(cr, w, h)
			gears.shape.rounded_rect(cr, w, h, radius)
		end,
		border_width = border_w,
		border_color = border_c,
		bg = pick(th.dialog_bg, "#000000CC"),
		widget = wibox.container.background,
	})
end

return M
