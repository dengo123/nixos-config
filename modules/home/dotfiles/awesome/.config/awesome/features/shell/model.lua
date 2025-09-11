-- ~/.config/awesome/features/shell/model.lua
local wibox = require("wibox")

-- Sub-Widgets
local Tabs = require("features.shell.widgets.tabs") -- verschmolzenes Tag/Task-Widget
local Clock = require("features.shell.widgets.clock")
local Prompt = require("features.shell.widgets.prompt")
local Systray = require("features.shell.widgets.systray")

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
	local tabs = Tabs.build(s, { modkey = modkey }) -- liefert { taglist = ... }
	local tray = showtray and Systray.build({ base_size = 20 }) or nil
	local clock = Clock.build("%H:%M") -- inkl. Popup-Kalender & Margin

	-- LINKS: Launcher · Tabs (taglist) · Prompt
	local left = {
		layout = wibox.layout.fixed.horizontal,
		spacing = 8,
		launcher,
		tabs.taglist,
		s.mypromptbox,
	}

	-- MITTE: (leer) – oder z.B. Separator/Spacer, falls deine View das erwartet
	local center = nil

	-- RECHTS: Keyboardlayout · Systray · Clock · Layoutbox
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
