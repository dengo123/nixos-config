-- ~/.config/awesome/widgets/wibar.lua
local awful = require("awful")
local wibox = require("wibox")
local beautiful = require("beautiful")
local taskmods = require("widgets.tasklist")

awful.screen.connect_for_each_screen(function(s)
	-- Widgets
	s.mypromptbox = awful.widget.prompt()
	s.tagbuttons = taskmods.tag_taskbuttons(s) -- XP‑artige Tag‑Buttons links
	s.tasklist = taskmods.tasklist(s) -- Fenster des aktiven Tags

	-- Wibar
	s.mywibar = awful.wibar({ position = "top", screen = s, height = beautiful.wibar_height })

	s.mywibar:setup({
		layout = wibox.layout.align.horizontal,
		{ -- links: Tag‑Buttons + Prompt
			layout = wibox.layout.fixed.horizontal,
			s.tagbuttons,
			s.mypromptbox,
		},
		-- mitte: Tasklist wie XP
		s.tasklist,
		{ -- rechts: Tray + Uhr
			layout = wibox.layout.fixed.horizontal,
			wibox.widget.systray(),
			wibox.widget.textclock("%a %d.%m.  %H:%M "),
		},
	})
end)
