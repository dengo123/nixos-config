-- ~/.config/awesome/shell/bar/model.lua
local wibox = require("wibox")

local Tabs = require("shell.bar.widgets.tabs")
local Clock = require("shell.bar.widgets.clock")
local Systray = require("shell.bar.widgets.systray")
local Start = require("shell.bar.widgets.start") -- <- NEU

local M = {}

function M.build(s, opts)
	opts = opts or {}
	local cfg = opts.cfg or {}
	local modkey = cfg.modkey or "Mod4"
	local kb = opts.keyboardlayout
	local showtray = (opts.systray ~= false)

	local tabs = Tabs.build(s, { modkey = modkey })
	local tray = showtray and Systray.build({ base_size = 20 }) or nil
	local clock = Clock.build("%H:%M")

	local start_btn = Start.build(s, {
		menu = cfg.mymainmenu,
		label = "Start",
		icon = cfg.start_icon or cfg.awesome_icon,
	})

	local left = {
		layout = wibox.layout.fixed.horizontal,
		spacing = 8,
		start_btn,
		tabs.taglist,
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
