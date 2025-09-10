-- ~/.config/awesome/widgets/bar.lua
local gears = require("gears")
local awful = require("awful")
local wibox = require("wibox")
local beautiful = require("beautiful")

local Tabs = require("widgets.bar.tabs")
local Sections = require("widgets.bar.sections")
local Prompt = require("widgets.bar.prompt")
local Layoutbox = require("widgets.bar.layoutbox")

local M = {}

function M.setup(s, opts)
	local cfg = (opts and opts.cfg) or {}
	local modkey = cfg.modkey
	local mylauncher = opts.launcher
	local mykeyboardlayout = opts.keyboardlayout
	local show_systray = opts.systray ~= false -- default: true

	-- Basics
	local mytextclock = wibox.widget.textclock("%a %d.%m.  %H:%M")

	-- Prompt + Layoutbox
	s.mypromptbox = Prompt.build()

	s.mylayoutbox = Layoutbox.build(s)

	-- Tabs (Taglist + Tasklist)
	local tabs = Tabs.build(s, { modkey = modkey })

	-- Sektionen zusammenstellen
	local left = Sections.left({
		launcher = mylauncher,
		taglist = tabs.taglist,
		prompt = s.mypromptbox,
	})

	local center = Sections.center({
		tasklist = tabs.tasklist,
	})

	local right = Sections.right({
		kbd = mykeyboardlayout,
		systray = show_systray,
		clock = mytextclock,
		layoutbox = s.mylayoutbox,
	})

	-- Bar
	s.mywibox = awful.wibar({
		position = "bottom",
		screen = s,
		height = beautiful.wibar_height or 28,
		bg = beautiful.wibar_bg,
		fg = beautiful.wibar_fg,
	})

	s.mywibox:setup({
		layout = wibox.layout.align.horizontal,
		left,
		center,
		right,
	})
end

return M
