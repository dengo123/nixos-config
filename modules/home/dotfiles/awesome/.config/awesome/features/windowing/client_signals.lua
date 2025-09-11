-- ~/.config/awesome/features/windowing/client_signals.lua
local awful = require("awful")
local beautiful = require("beautiful")

local M = {}

function M.apply(o)
	local sloppy = o.sloppy_focus
	local taskbar = o.taskbar
	local mouse = o.mouse
	local tb_opts = o.titlebar_opts or {}

	client.connect_signal("manage", function(c)
		if awesome.startup and not c.size_hints.user_position and not c.size_hints.program_position then
			awful.placement.no_offscreen(c)
		end
	end)

	client.connect_signal("request::titlebars", function(c)
		taskbar.attach(c, mouse, tb_opts)
	end)

	if sloppy then
		client.connect_signal("mouse::enter", function(c)
			c:emit_signal("request::activate", "mouse_enter", { raise = false })
		end)
	end

	client.connect_signal("focus", function(c)
		c.border_color = beautiful.border_focus
	end)
	client.connect_signal("unfocus", function(c)
		c.border_color = beautiful.border_normal
	end)
end

return M
