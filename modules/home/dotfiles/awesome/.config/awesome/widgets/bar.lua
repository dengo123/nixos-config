-- ~/.config/awesome/bar.lua
local gears = require("gears")
local awful = require("awful")
local wibox = require("wibox")
local beautiful = require("beautiful")

local M = {}

-- 1:1 aus rc.lua
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

local function set_wallpaper(s)
	if beautiful.wallpaper then
		local wallpaper = beautiful.wallpaper
		if type(wallpaper) == "function" then
			wallpaper = wallpaper(s)
		end
		gears.wallpaper.maximized(wallpaper, s, true)
	end
end

-- bei Geometrieänderung Wallpaper neu setzen (wie vorher in rc.lua)
screen.connect_signal("property::geometry", set_wallpaper)

-- Öffentliche Setup-Funktion: baut die Bar für Screen s
function M.setup(s, opts)
	opts = opts or {}
	local launcher = opts.launcher -- erwartet mylauncher
	local keyboardlayout = opts.keyboardlayout -- erwartet mykeyboardlayout

	-- Wallpaper (wie vorher)
	set_wallpaper(s)

	-- Promptbox & Layoutbox
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

	-- Taglist & Tasklist
	s.mytaglist = awful.widget.taglist({
		screen = s,
		filter = awful.widget.taglist.filter.all,
		buttons = taglist_buttons,
	})

	s.mytasklist = awful.widget.tasklist({
		screen = s,
		filter = awful.widget.tasklist.filter.currenttags,
		buttons = tasklist_buttons,
	})

	-- Wibar (Position unverändert: "top")
	s.mywibox = awful.wibar({ position = "top", screen = s })

	-- Bar-Befüllung (links / mitte / rechts)
	s.mywibox:setup({
		layout = wibox.layout.align.horizontal,
		{ -- links
			layout = wibox.layout.fixed.horizontal,
			launcher, -- <- mylauncher aus rc.lua
			s.mytaglist,
			s.mypromptbox,
		},
		s.mytasklist, -- mitte
		{ -- rechts
			layout = wibox.layout.fixed.horizontal,
			keyboardlayout, -- <- mykeyboardlayout aus rc.lua
			wibox.widget.systray(),
			mytextclock,
			s.mylayoutbox,
		},
	})
end

return M
