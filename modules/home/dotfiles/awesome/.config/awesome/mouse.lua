-- ~/.config/awesome/mouse.lua
local gears = require("gears")
local awful = require("awful")

local M = {}

-- Root-Mausbindungen (Rechtsklick Menü, Scroll Tag vor/zurück)
function M.apply_root(mymainmenu)
	root.buttons(gears.table.join(
		awful.button({}, 3, function()
			if mymainmenu then
				mymainmenu:toggle()
			end
		end),
		awful.button({}, 4, awful.tag.viewnext),
		awful.button({}, 5, awful.tag.viewprev)
	))
end

-- Buttons, die an Clients gebunden werden (für rules.properties.buttons)
function M.client_buttons()
	return gears.table.join(
		awful.button({}, 1, function(c)
			c:emit_signal("request::activate", "mouse_click", { raise = true })
		end),
		awful.button({ "Mod4" }, 1, function(c)
			c:emit_signal("request::activate", "mouse_click", { raise = true })
			awful.mouse.client.move(c)
		end),
		awful.button({ "Mod4" }, 3, function(c)
			c:emit_signal("request::activate", "mouse_click", { raise = true })
			awful.mouse.client.resize(c)
		end)
	)
end

-- Buttons speziell für Titlebars (hier binden wir den übergebenen Client)
function M.titlebar_buttons(c_arg)
	return gears.table.join(
		awful.button({}, 1, function()
			local c = c_arg or client.focus
			if not c then
				return
			end
			c:emit_signal("request::activate", "titlebar", { raise = true })
			awful.mouse.client.move(c)
		end),
		awful.button({}, 3, function()
			local c = c_arg or client.focus
			if not c then
				return
			end
			c:emit_signal("request::activate", "titlebar", { raise = true })
			awful.mouse.client.resize(c)
		end)
	)
end

return M
