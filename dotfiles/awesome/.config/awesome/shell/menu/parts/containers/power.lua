-- ~/.config/awesome/shell/menu/dialogs/containers/power.lua
local wibox = require("wibox")
local gears = require("gears")

local M = {}

-- slots = { header=?, body=?, footer=? }  -> alles fertige Widgets
function M.build(th, dims, slots)
	local header = slots.header or wibox.widget({ layout = wibox.layout.fixed.horizontal })
	local body = slots.body or wibox.widget({ layout = wibox.layout.fixed.horizontal })
	local footer = slots.footer or wibox.widget({ layout = wibox.layout.fixed.horizontal })

	return wibox.widget({
		{
			{ header, strategy = "exact", height = dims.header_h, widget = wibox.container.constraint },
			{ body, strategy = "exact", height = dims.body_h, widget = wibox.container.constraint },
			{ footer, strategy = "exact", height = dims.footer_h, widget = wibox.container.constraint },
			layout = wibox.layout.fixed.vertical,
		},
		shape = gears.shape.rounded_rect,
		bg = th.dialog_bg or "#00000000",
		widget = wibox.container.background,
	})
end

return M
