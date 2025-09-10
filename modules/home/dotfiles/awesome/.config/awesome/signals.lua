-- ~/.config/awesome/signals.lua
local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
local beautiful = require("beautiful")

local M = {}

-- mouse: dein require("mouse")-Modul (für titlebar buttons)
function M.apply(mouse)
	-- New client appears
	client.connect_signal("manage", function(c)
		if awesome.startup and not c.size_hints.user_position and not c.size_hints.program_position then
			awful.placement.no_offscreen(c)
		end
	end)

	-- Titlebars
	client.connect_signal("request::titlebars", function(c)
		local buttons = mouse.titlebar_buttons(c)
		awful.titlebar(c):setup({
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
	end)

	-- Focus follows mouse (sloppy)
	client.connect_signal("mouse::enter", function(c)
		c:emit_signal("request::activate", "mouse_enter", { raise = false })
	end)

	client.connect_signal("focus", function(c)
		c.border_color = beautiful.border_focus
	end)
	client.connect_signal("unfocus", function(c)
		c.border_color = beautiful.border_normal
	end)
end

return M
