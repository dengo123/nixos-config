-- features/shell/model.lua

local wibox = require("wibox")

-- Sub-Widgets (NEU: richtiger Pfad)
local Layoutbox = require("features.shell.widgets.layoutbox")
local Clock = require("features.shell.widgets.clock")
local Prompt = require("features.shell.widgets.prompt")
local Systray = require("features.shell.widgets.systray")
local Tabs = require("features.shell.widgets.tabs")

local M = {}

-- baut linke/mittlere/rechte Seite für eine Bar
function M.build(s, opts)
	opts = opts or {}
	local cfg = opts.cfg or {}
	local modkey = cfg.modkey or "Mod4"
	local launcher = opts.launcher
	local kb = opts.keyboardlayout
	local showtray = (opts.systray ~= false)

	-- Widgets
	s.mypromptbox = Prompt.build()
	s.mylayoutbox = Layoutbox.build(s)
	local tabs = Tabs.build(s, { modkey = modkey })
	local tray = showtray and Systray.build({ base_size = 20 }) or nil
	local clock = Clock.build("%a %d.%m.  %H:%M")

	local left = {
		layout = wibox.layout.fixed.horizontal,
		spacing = 8,
		launcher,
		tabs.taglist,
		s.mypromptbox,
	}

	local center = tabs.tasklist

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
