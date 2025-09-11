-- ~/.config/awesome/features/windowing/taskbar.lua
local awful = require("awful")
local wibox = require("wibox")

local M = {}

function M.attach(c, mouse, opts)
	local buttons = mouse and mouse.titlebar_buttons and mouse.titlebar_buttons(c) or nil
	local pos = (opts and opts.position) or "top"
	local size = (opts and opts.size) or 28

	awful.titlebar(c, { position = pos, size = size }):setup({
		{
			awful.titlebar.widget.iconwidget(c),
			buttons = buttons,
			layout = wibox.layout.fixed.horizontal,
		},
		{
			{ align = "center", widget = awful.titlebar.widget.titlewidget(c) },
			buttons = buttons,
			layout = wibox.layout.flex.horizontal,
		},
		{
			awful.titlebar.widget.floatingbutton(c),
			awful.titlebar.widget.maximizedbutton(c),
			awful.titlebar.widget.stickybutton(c),
			awful.titlebar.widget.ontopbutton(c),
			awful.titlebar.widget.closebutton(c),
			layout = wibox.layout.fixed.horizontal,
		},
		layout = wibox.layout.align.horizontal,
	})
end

return M
