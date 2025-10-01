-- ~/.config/awesome/shell/bar/init.lua
local awful = require("awful")
local wibox = require("wibox")
local gears = require("gears")
local beautiful = require("beautiful")

local Tabs = require("shell.bar.widgets.tabs")
local Tags = require("shell.bar.widgets.tags")
local Clock = require("shell.bar.widgets.clock")
local Systray = require("shell.bar.widgets.systray")
local Start = require("shell.bar.widgets.start")

local M = {}

-- Hilfsfunktion: Theme-Teil über ui.theme.<name>.get(...) holen (optional overrides)
local function get_part_theme(ui, name, overrides)
	overrides = overrides or {}
	local t = ui and ui.theme and ui.theme[name]
	if t and type(t.get) == "function" then
		return t.get(overrides)
	end
	return nil
end

function M.build(s, opts)
	opts = opts or {}
	local cfg = opts.cfg or {}
	local ui = opts.ui

	local modkey = cfg.modkey or "Mod4"
	local showtray = (cfg.systray ~= false)

	-- Themes NUR via ui.theme beziehen
	local TabsTheme = get_part_theme(ui, "tabs", cfg.tabs or {})
	local StartTheme = get_part_theme(ui, "start", cfg.start or {})
	local MenuTheme = get_part_theme(ui, "menu", cfg.menu or cfg.menus or {}) -- falls du Menü-Theming nutzt

	-- Widgets
	local tabs = Tabs.build(s, {
		modkey = modkey,
		group_by_class = true,
		theme = TabsTheme, -- <- injiziert, kein require im Widget
		menu_theme = MenuTheme,
	})

	local tags = Tags.build(s, {}) -- (falls Tags auch Theme wollen, analog ergänzen)

	local tray = showtray
			and Systray.build({
				menu_theme = MenuTheme, -- reine UI; Farben/Größen aus wibar-Theme via beautiful.*
			})
		or nil

	local clock = Clock.build() -- Format etc. aus theme/wibar.lua via beautiful.*

	local start_btn = Start.build({
		screen = s,
		theme = assert(StartTheme, "ui.theme.start.get(...) returned nil"), -- klare Fehlermeldung
		launcher = cfg.launcher,
		terminal = cfg.terminal,
		menu = cfg.mymainmenu,
	})

	-- Layoutbox
	if not s.mylayoutbox or not s.mylayoutbox.valid then
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
	end

	local lb = wibox.widget({
		s.mylayoutbox,
		left = beautiful.layoutbox_pad_h or 0,
		right = beautiful.layoutbox_pad_h or 0,
		top = beautiful.layoutbox_pad_v or 0,
		bottom = beautiful.layoutbox_pad_v or 0,
		widget = wibox.container.margin,
	})

	local left = {
		layout = wibox.layout.fixed.horizontal,
		spacing = 8,
		start_btn,
		tags.indicator,
		tabs.tasklist,
	}

	local center = nil

	local right = {
		layout = wibox.layout.fixed.horizontal,
		spacing = 0,
		lb,
		tray,
		clock,
	}

	return { left = left, center = center, right = right }
end

function M.place(s, opts)
	local pos = assert(beautiful.wibar_position, "wibar_position not set by theme")
	local h = assert(beautiful.wibar_height, "wibar_height not set by theme")
	local bg = assert(beautiful.wibar_bg, "wibar_bg not set by theme")
	local fg = assert(beautiful.wibar_fg, "wibar_fg not set by theme")

	local model = M.build(s, opts)

	s.mywibox = awful.wibar({
		position = pos,
		screen = s,
		height = h,
		bg = bg,
		fg = fg,
		ontop = (beautiful.wibar_on_top == true) or false,
		opacity = beautiful.wibar_opacity or 1.0,
		shape = beautiful.wibar_shape,
	})

	local m = beautiful.wibar_margins
	if type(m) == "table" then
		s.mywibox:struts({
			top = m.top or 0,
			right = m.right or 0,
			bottom = m.bottom or 0,
			left = m.left or 0,
		})
	end

	s.mywibox:setup({
		layout = wibox.layout.align.horizontal,
		model.left,
		model.center,
		model.right,
	})

	return s.mywibox
end

M.setup = M.place
return M
