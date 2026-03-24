-- ~/.config/awesome/shell/notify/center/view.lua
local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")

local M = {}

-- =========================================================================
-- Public API
-- =========================================================================

function M.build(args)
	local widget = args.widget
	local popup_width = tonumber(args.popup_width) or 1
	local popup_height = tonumber(args.popup_height) or 1

	local pad_top = tonumber(args.pad_top) or 0
	local pad_right = tonumber(args.pad_right) or 0
	local pad_bottom = tonumber(args.pad_bottom) or 0
	local pad_left = tonumber(args.pad_left) or 0

	local bg = args.bg or "#00000000"

	if not widget then
		return nil
	end

	local padded = wibox.container.margin(widget)
	padded.top = pad_top
	padded.right = pad_right
	padded.bottom = pad_bottom
	padded.left = pad_left

	local viewport_height = math.max(1, popup_height - pad_top - pad_bottom)

	local bottom_right = wibox.widget({
		padded,
		halign = "right",
		valign = "bottom",
		widget = wibox.container.place,
	})

	local viewport = wibox.widget({
		bottom_right,
		strategy = "exact",
		width = popup_width,
		height = viewport_height,
		widget = wibox.container.constraint,
	})

	local fill = wibox.widget({
		viewport,
		forced_width = popup_width,
		forced_height = popup_height,
		widget = wibox.container.constraint,
	})

	local panel = wibox.container.background(fill)
	panel.bg = bg

	panel:buttons(gears.table.join(
		awful.button({}, 4, function()
			awesome.emit_signal("notify::center_scroll_up")
		end),
		awful.button({}, 5, function()
			awesome.emit_signal("notify::center_scroll_down")
		end)
	))

	return panel
end

return M
