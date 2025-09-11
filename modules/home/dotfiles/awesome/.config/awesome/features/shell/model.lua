-- ~/.config/awesome/features/shell/model.lua
local wibox = require("wibox")

local Tabs = require("features.shell.widgets.tabs")
local Clock = require("features.shell.widgets.clock")
local Systray = require("features.shell.widgets.systray")
local Start = require("features.shell.widgets.start") -- <- NEU

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
