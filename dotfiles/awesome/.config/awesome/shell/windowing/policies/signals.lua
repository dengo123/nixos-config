-- ~/.config/awesome/shell/windowing/policies/signals.lua
local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")

local M = {}

function M.apply(o)
	o = o or {}
	local MinStack = o.minimize_stack
	local focus = o.focus
	local container = o.container
	local attach_titlebar = o.attach_titlebar

	-- Titlebar (unverändert)
	client.connect_signal("request::titlebars", function(c)
		if type(attach_titlebar) == "function" then
			attach_titlebar(c)
		end
	end)

	-- Focus-Policy
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

	-- Container Styling
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

	-- === Minimize-Stack ===
	if MinStack then
		client.connect_signal("property::minimized", function(c)
			if c.minimized then
				MinStack.push(c)
			else
				MinStack.remove(c)
			end
		end)
		client.connect_signal("unmanage", function(c)
			MinStack.remove(c)
		end)

		awesome.connect_signal("windowing::restore_request", function(s)
			s = s or awful.screen.focused()

			-- 1) eigener Stack
			local r = MinStack.pop_on_screen(s)

			-- 2) Fallback: irgendeinen minimierten auf dem Screen finden
			if not r then
				for cl in
					awful.client.iterate(function(x)
						return x and x.valid and x.minimized and x.screen == s
					end, nil, s)
				do
					r = cl
					break
				end
			end

			-- 3) letzter Fallback: awesome interner Restore-Stack
			if not r then
				r = awful.client.restore()
			end

			if r and r.valid then
				r.minimized = false
				r:emit_signal("request::activate", "key.unminimize", { raise = true })
			end
		end)
	end
end

return M
