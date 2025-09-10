-- ~/.config/awesome/widgets/bar/sections.lua
local wibox = require("wibox")

local M = {}

function M.left(args)
	return {
		layout = wibox.layout.fixed.horizontal,
		spacing = 8,
		args.launcher,
		args.taglist,
		args.prompt,
	}
end

function M.center(args)
	-- nur Tasklist in der Mitte
	return args.tasklist
end

function M.right(args)
	return {
		layout = wibox.layout.fixed.horizontal,
		spacing = 10,
		args.kbd,
		(args.systray and wibox.widget.systray() or nil),
		args.clock,
		args.layoutbox,
	}
end

return M
