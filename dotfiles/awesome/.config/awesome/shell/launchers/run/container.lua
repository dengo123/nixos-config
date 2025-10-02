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

-- build(th, dims, { title, body, cancel_btn, footer })  -- footer optional
function M.build(th, dims, w)
	local radius = pick(th.panel_radius, 12)
	local border_w = pick(th.panel_border_width, 2)
	local border_color = pick(th.panel_border, pick(th.header_bg, "#235CDB"))
	local header_h = pick(th.panel_header_h, 28)

	-- Header: Titel links, X rechts (optional cancel)
	local title = wibox.widget({
		text = w.title or "Launcher",
		align = "left",
		valign = "center",
		widget = wibox.widget.textbox,
	})
	local closeX = wibox.widget({
		{ text = "✖", align = "center", valign = "center", widget = wibox.widget.textbox },
		forced_width = 28,
		forced_height = 22,
		shape = gears.shape.rounded_bar,
		bg = "#00000000",
		widget = wibox.container.background,
	})
	if w.cancel_btn and w.cancel_btn.activate then
		closeX:buttons(gears.table.join(require("awful").button({}, 1, function()
			w.cancel_btn:activate()
		end)))
	end

	local header = wibox.widget({
		{
			{ title, halign = "left", widget = wibox.container.place },
			{ closeX, halign = "right", widget = wibox.container.place },
			layout = wibox.layout.align.horizontal,
		},
		left = pick(th.header_pad_h, th.pad_h, 12),
		right = pick(th.header_pad_h, th.pad_h, 8),
		widget = wibox.container.margin,
	})
	header = wibox.widget({
		header,
		bg = pick(th.panel_header_bg, pick(th.header_bg, "#235CDB")),
		fg = pick(th.panel_header_fg, pick(th.header_fg, "#FFFFFF")),
		widget = wibox.container.background,
	})

	local body = wibox.widget({
		{
			w.body,
			left = pick(th.panel_pad_h, th.pad_h, 12),
			right = pick(th.panel_pad_h, th.pad_h, 12),
			top = pick(th.panel_pad_v, th.pad_v, 12),
			bottom = pick(th.panel_pad_v, th.pad_v, 12),
			widget = wibox.container.margin,
		},
		bg = pick(th.panel_body_bg, pick(th.body_bg, "#1d2f6f")),
		fg = pick(th.panel_body_fg, pick(th.body_fg, "#FFFFFF")),
		widget = wibox.container.background,
	})

	-- Höhen
	dims.header_h = header_h
	dims.body_h = math.max(0, dims.h - dims.header_h - (w.footer and dims.footer_h or 0))

	local stack = wibox.widget({
		{ header, strategy = "exact", height = dims.header_h, widget = wibox.container.constraint },
		{ body, strategy = "exact", height = dims.body_h, widget = wibox.container.constraint },
		layout = wibox.layout.fixed.vertical,
	})

	if w.footer then
		stack = wibox.widget({
			stack,
			{ w.footer, strategy = "exact", height = dims.footer_h, widget = wibox.container.constraint },
			layout = wibox.layout.fixed.vertical,
		})
	end

	return wibox.widget({
		stack,
		shape = function(cr, w_, h_)
			gears.shape.rounded_rect(cr, w_, h_, radius)
		end,
		border_width = border_w,
		border_color = border_color,
		bg = pick(th.panel_bg, pick(th.dialog_bg, "#000000AA")),
		widget = wibox.container.background,
	})
end

return M
