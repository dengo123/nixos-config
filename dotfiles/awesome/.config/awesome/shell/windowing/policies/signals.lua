-- windowing/policies/signals.lua
local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")

local M = {}

function M.apply(o)
	o = o or {}
	local attach_titlebar = o.attach_titlebar
	local focus = o.focus
	local container = o.container
	local MinStack = o.minimize_stack

	-- Titlebar anfordern
	client.connect_signal("request::titlebars", function(c)
		if type(attach_titlebar) == "function" then
			attach_titlebar(c)
		else
			local buttons = gears.table.join(
				awful.button({}, 1, function()
					c:emit_signal("request::activate", "titlebar", { raise = true })
					awful.mouse.client.move(c)
				end),
				awful.button({}, 3, function()
					c:emit_signal("request::activate", "titlebar", { raise = true })
					awful.mouse.client.resize(c)
				end)
			)
			awful.titlebar(c):setup({
				{
					awful.titlebar.widget.iconwidget(c),
					buttons = buttons,
					layout = wibox.layout.fixed.horizontal,
				},
				{ align = "center", widget = awful.titlebar.widget.titlewidget(c) },
				{ layout = wibox.layout.fixed.horizontal },
				layout = wibox.layout.align.horizontal,
			})
		end
	end)

	-- Focus-Policy (sloppy + cursor-center)
	if focus and focus.on_mouse_enter then
		client.connect_signal("mouse::enter", function(c)
			focus:on_mouse_enter(c)
		end)
	end
	if focus and focus.on_focus then
		client.connect_signal("focus", function(c)
			focus:on_focus(c)
		end)
	end

	-- Container-Styling
	local function restyle(c)
		if container and container.apply then
			container:apply(c)
		end
	end
	client.connect_signal("manage", function(c)
		gears.timer.delayed_call(restyle, c)
	end)
	client.connect_signal("focus", restyle)
	client.connect_signal("unfocus", restyle)
	for _, p in ipairs({ "maximized", "maximized_vertical", "maximized_horizontal", "fullscreen", "minimized" }) do
		client.connect_signal("property::" .. p, restyle)
	end
	screen.connect_signal("arrange", function(s)
		for _, c in ipairs(s.clients) do
			restyle(c)
		end
	end)

	-- Minimize-Stack housekeeping
	if MinStack and MinStack.remove then
		client.connect_signal("unmanage", function(c)
			MinStack.remove(c)
		end)
		client.connect_signal("property::minimized", function(c)
			if not c.minimized then
				MinStack.remove(c)
			end
		end)
	end
end

return M
