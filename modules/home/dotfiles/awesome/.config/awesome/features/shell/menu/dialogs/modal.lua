local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")

local M = {}

function M.show(args)
	args = args or {}
	local s = awful.screen.focused()
	local wa = s.workarea or s.geometry

	local close_ref = { backdrop = nil, dialog = nil }

	local backdrop = wibox({
		screen = s,
		type = "dock",
		ontop = true,
		visible = true,
		x = wa.x,
		y = wa.y,
		width = wa.width,
		height = wa.height,
		bg = args.backdrop or "#00000088",
	})
	backdrop:buttons(gears.table.join(awful.button({}, 1, function()
		if close_ref.dialog then
			close_ref.dialog.visible = false
		end
		if close_ref.backdrop then
			close_ref.backdrop.visible = false
		end
	end)))

	local dialog = wibox({
		screen = s,
		type = "splash",
		ontop = true,
		visible = true,
		width = args.width or 420,
		height = args.height or 160,
		bg = args.bg or "#222222",
		fg = args.fg or "#ffffff",
		shape = function(cr, w, h)
			gears.shape.rounded_rect(cr, w, h, args.radius or 6)
		end,
	})

	dialog:setup({
		{
			args.content,
			margins = 16,
			widget = wibox.container.margin,
		},
		widget = wibox.container.background,
	})

	awful.placement.centered(dialog, { parent = wa })

	close_ref.backdrop = backdrop
	close_ref.dialog = dialog

	return function()
		if close_ref.dialog then
			close_ref.dialog.visible = false
		end
		if close_ref.backdrop then
			close_ref.backdrop.visible = false
		end
	end
end

return M
