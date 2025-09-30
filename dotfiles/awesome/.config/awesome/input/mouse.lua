-- ~/.config/awesome/mouse.lua
local gears, awful = require("gears"), require("awful")
local M = {}

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

function M.client_buttons(modkey) -- <— modkey reinreichen
	return gears.table.join(
		awful.button({}, 1, function(c)
			c:emit_signal("request::activate", "mouse_click", { raise = true })
		end),
		awful.button({ modkey }, 1, function(c)
			c:emit_signal("request::activate", "mouse_click", { raise = true })
			awful.mouse.client.move(c)
		end),
		awful.button({ modkey }, 3, function(c)
			c:emit_signal("request::activate", "mouse_click", { raise = true })
			awful.mouse.client.resize(c)
		end)
	)
end

function M.titlebar_buttons(c)
	return gears.table.join(
		awful.button({}, 1, function()
			c:emit_signal("request::activate", "titlebar", { raise = true })
			awful.mouse.client.move(c)
		end),
		awful.button({}, 3, function()
			c:emit_signal("request::activate", "titlebar", { raise = true })
			awful.mouse.client.resize(c)
		end)
	)
end

return M
