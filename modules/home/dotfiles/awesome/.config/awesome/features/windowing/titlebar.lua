-- ~/.config/awesome/features/windowing/titlebar.lua
local awful = require("awful")
local wibox = require("wibox")
local gears = require("gears")
local beautiful = require("beautiful")

local M = {}

function M.attach(c, mouse, opts)
	-- Maus-Buttons für Titelbereich (Move/Resize)
	local buttons = mouse and mouse.titlebar_buttons and mouse.titlebar_buttons(c)
		or gears.table.join(
			awful.button({}, 1, function()
				c:emit_signal("request::activate", "titlebar", { raise = true })
				awful.mouse.client.move(c)
			end),
			awful.button({}, 3, function()
				c:emit_signal("request::activate", "titlebar", { raise = true })
				awful.mouse.client.resize(c)
			end)
		)

	local pos = (opts and opts.position) or "top"
	local size = (opts and opts.size) or 28

	local tb = awful.titlebar(c, { position = pos, size = size })

	-- Optionale Sichtbarkeits-Hilfe (auskommentieren, wenn störend)
	local bg_normal = beautiful.titlebar_bg_normal or beautiful.bg_normal or "#222222"
	local fg_normal = beautiful.titlebar_fg_normal or beautiful.fg_normal or "#ffffff"

	tb.widget = {
		{ -- LEFT: Icon
			awful.titlebar.widget.iconwidget(c),
			buttons = buttons,
			layout = wibox.layout.fixed.horizontal,
		},

		{ -- MIDDLE: Titel (flex = nimmt Restbreite ein)
			{
				align = "center",
				widget = awful.titlebar.widget.titlewidget(c),
			},
			buttons = buttons,
			layout = wibox.layout.flex.horizontal,
		},

		{ -- RIGHT: Minimize | Maximize | Close
			awful.titlebar.widget.minimizebutton(c),
			awful.titlebar.widget.maximizedbutton(c),
			awful.titlebar.widget.closebutton(c),
			layout = wibox.layout.fixed.horizontal,
		},

		layout = wibox.layout.align.horizontal,
		-- Debug: farbiger Hintergrund, damit sichtbar
		bg = bg_normal,
		fg = fg_normal,
		widget = wibox.container.background,
	}
end

return M
