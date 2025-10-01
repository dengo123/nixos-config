-- ~/.config/awesome/shell/bar/model.lua
local awful = require("awful")
local gears = require("gears")
local beautiful = require("beautiful")
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
	local ui = opts.ui
	local modkey = cfg.modkey or "Mod4"
	local showtray = (opts.systray ~= false)

	-- === THEMES zentral aus ui.theme holen ===
	local theme = ui and ui.theme or nil

	local TabsTheme = (theme and theme.tabs and theme.tabs.get) and theme.tabs.get(cfg.tabs or {}) or nil
	local StartTheme = (theme and theme.start and theme.start.get) and theme.start.get(cfg.start or {}) or nil
	local MenuTheme = cfg.menus

	-- === Widgets ===
	local tabs = Tabs.build(s, {
		modkey = modkey,
		group_by_class = true,
		theme = TabsTheme,
		menu_theme = MenuTheme,
	})

	local tags = Tags.build(s, {})

	local tray = showtray and Systray.build({ menu_theme = MenuTheme }) or nil

	local clock = Clock.build() -- Format/Refresh aus Theme

	-- Start-Button
	local start_btn = Start.build({
		screen = s,
		theme = StartTheme,
		launcher = cfg.launcher,
		terminal = cfg.terminal,
		menu = cfg.mymainmenu,
	})

	-- === Layoutbox statt keyboardlayout ===
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

	-- === Sektionen ===
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
		lb, -- <- hier statt kb
		tray,
		clock,
	}

	return { left = left, center = center, right = right }
end

return M
