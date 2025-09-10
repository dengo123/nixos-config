-- ~/.config/awesome/widgets/bar.lua
local gears, awful, wibox, beautiful = require("gears"), require("awful"), require("wibox"), require("beautiful")
local M = {}

function M.setup(s, opts)
	local cfg = opts.cfg or {}
	local modkey = cfg.modkey
	local mylauncher = opts.launcher
	local mykeyboardlayout = opts.keyboardlayout

	local mytextclock = wibox.widget.textclock()

	local taglist_buttons = gears.table.join(
		awful.button({}, 1, function(t)
			t:view_only()
		end),
		awful.button({ modkey }, 1, function(t)
			if client.focus then
				client.focus:move_to_tag(t)
			end
		end),
		awful.button({}, 3, awful.tag.viewtoggle),
		awful.button({ modkey }, 3, function(t)
			if client.focus then
				client.focus:toggle_tag(t)
			end
		end),
		awful.button({}, 4, function(t)
			awful.tag.viewnext(t.screen)
		end),
		awful.button({}, 5, function(t)
			awful.tag.viewprev(t.screen)
		end)
	)

	local tasklist_buttons = gears.table.join(
		awful.button({}, 1, function(c)
			if c == client.focus then
				c.minimized = true
			else
				c:emit_signal("request::activate", "tasklist", { raise = true })
			end
		end),
		awful.button({}, 3, function()
			awful.menu.client_list({ theme = { width = 250 } })
		end),
		awful.button({}, 4, function()
			awful.client.focus.byidx(1)
		end),
		awful.button({}, 5, function()
			awful.client.focus.byidx(-1)
		end)
	)

	s.mypromptbox = awful.widget.prompt()
	s.mylayoutbox = awful.widget.layoutbox(s)
	s.mylayoutbox:buttons(gears.table.join(
		awful.button({}, 1, function()
			awful.layout.inc(1)
		end),
		awful.button({}, 3, function()
			awful.layout.inc(-1)
		end),
		awful.button({}, 4, function()
			awful.layout.inc(1)
		end),
		awful.button({}, 5, function()
			awful.layout.inc(-1)
		end)
	))
	s.mytaglist =
		awful.widget.taglist({ screen = s, filter = awful.widget.taglist.filter.all, buttons = taglist_buttons })
	s.mytasklist = awful.widget.tasklist({
		screen = s,
		filter = awful.widget.tasklist.filter.currenttags,
		buttons = tasklist_buttons,
	})

	s.mywibox = awful.wibar({ position = "top", screen = s })
	s.mywibox:setup({
		layout = wibox.layout.align.horizontal,
		{ layout = wibox.layout.fixed.horizontal, mylauncher, s.mytaglist, s.mypromptbox },
		s.mytasklist,
		{
			layout = wibox.layout.fixed.horizontal,
			mykeyboardlayout,
			wibox.widget.systray(),
			mytextclock,
			s.mylayoutbox,
		},
	})
end

return M
