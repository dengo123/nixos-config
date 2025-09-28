-- features/shell/menu/dialogs/parts/widgets/cancel.lua
local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")

local M = {}

function M.mk_cancel_button(label, on_click, th)
	th = th or {}

	local inner = wibox.widget({
		{
			{
				text = label or "Cancel",
				align = "center",
				valign = "center",
				widget = wibox.widget.textbox,
			},
			left = (th.cancel_pad_h or 12),
			right = (th.cancel_pad_h or 12),
			top = (th.cancel_pad_v or 8),
			bottom = (th.cancel_pad_v or 8),
			widget = wibox.container.margin,
		},
		bg = th.cancel_bg or "#334155",
		fg = th.cancel_fg or "#FFFFFF",
		shape = function(cr, w, h)
			local r = (th.cancel_radius or 8)
			if r > 0 then
				gears.shape.rounded_rect(cr, w, h, r)
			else
				gears.shape.rectangle(cr, w, h)
			end
		end,
		shape_border_width = 0,
		shape_border_color = "#00000000",
		widget = wibox.container.background,
	})

	local root = wibox.widget({ inner, widget = wibox.container.background })

	root:connect_signal("mouse::enter", function()
		inner.bg = th.cancel_hover_bg or inner.bg
		inner.shape_border_width = th.cancel_hover_bw or 2
		inner.shape_border_color = th.cancel_hover_border or "#2B77FF"
	end)
	root:connect_signal("mouse::leave", function()
		inner.bg = th.cancel_bg or "#334155"
		inner.shape_border_width = 0
		inner.shape_border_color = "#00000000"
	end)

	root:buttons(gears.table.join(awful.button({}, 1, function()
		if on_click then
			on_click()
		end
	end)))

	return root
end

return M
