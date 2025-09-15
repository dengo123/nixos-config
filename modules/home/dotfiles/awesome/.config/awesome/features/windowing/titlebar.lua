-- ~/.config/awesome/features/windowing/titlebar.lua
local awful = require("awful")
local wibox = require("wibox")

local M = {}

function M.attach(c, mouse, opts)
	local buttons = mouse and mouse.titlebar_buttons and mouse.titlebar_buttons(c) or nil
	local pos = (opts and opts.position) or "top"
	local size = (opts and opts.size) or 28

	awful.titlebar(c, { position = pos, size = size }):setup({
		-- LEFT: Icon + Title (Titel mit Move/Resize-Buttons)
		{
			layout = wibox.layout.fixed.horizontal,
			awful.titlebar.widget.iconwidget(c),
			{
				widget = awful.titlebar.widget.titlewidget(c),
				align = "center",
			},
			buttons = buttons,
		},

		-- MIDDLE: leer
		nil,

		-- RIGHT: (sichtbar rechts->links): Close | Floating | Minimize
		{
			layout = wibox.layout.fixed.horizontal,
			-- Reihenfolge im Code = links->rechts innerhalb des rechten Blocks.
			-- Der rechte Block selbst sitzt ganz rechts, daher Close am Ende = ganz rechts.
			awful.titlebar.widget.minimizebutton(c), -- links von den drei
			awful.titlebar.widget.floatingbutton(c), -- Mitte
			awful.titlebar.widget.closebutton(c), -- ganz rechts
		},

		layout = wibox.layout.align.horizontal,
	})
end

return M
