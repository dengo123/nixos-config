local wibox = require("wibox")

local Tabs = require("shell.bar.widgets.tabs")
local Tags = require("shell.bar.widgets.tags")
local Clock = require("shell.bar.widgets.clock")
local Systray = require("shell.bar.widgets.systray")
local Start = require("shell.bar.widgets.start")

local M = {}

function M.build(s, opts)
	opts = opts or {}
	local cfg = opts.cfg or {}
	local modkey = cfg.modkey or "Mod4"
	local kb = opts.keyboardlayout
	local showtray = (opts.systray ~= false)

	local tabs = Tabs.build(s, { modkey = modkey }) -- tasks-only tabs
	local tags = Tags.build(s, {}) -- neuer Indicator
	local tray = showtray and Systray.build() or nil
	local clock = Clock.build("%H:%M")

	local start_btn = Start.build(s, {
		label = "Start",
		icon = cfg.start_icon or cfg.awesome_icon,
		menu = cfg.mymainmenu, -- kann nil sein
		on_press = cfg.launcher_fn, -- externer Launcher (optional)
	})

	local left = {
		layout = wibox.layout.fixed.horizontal,
		spacing = 8,
		start_btn,
		tags.indicator, -- sitzt zwischen Start und Tabs
		tabs.tasklist,
	}

	local center = nil

	local right = {
		layout = wibox.layout.fixed.horizontal,
		spacing = 10,
		kb,
		tray,
		clock,
		s.mylayoutbox,
	}

	return { left = left, center = center, right = right }
end

return M
