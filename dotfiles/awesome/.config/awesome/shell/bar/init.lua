-- ~/.config/awesome/shell/bar/init.lua
local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
local beautiful = require("beautiful")

local Tabs = require("shell.bar.widgets.tabs")
local Tags = require("shell.bar.widgets.tags")
local Clock = require("shell.bar.widgets.clock")
local Systray = require("shell.bar.widgets.systray")
local Start = require("shell.bar.widgets.start")

local M = {}

-- Baut die Bar für einen Screen. Erwartet args = { cfg, ui, systray? }
function M.setup(s, args)
	args = args or {}
	local cfg = args.cfg or {}
	local ui = args.ui
	local modkey = cfg.modkey or "Mod4"
	local showtray = (args.systray ~= false)

	-- Theme-Module (mit Fallback, falls ui.theme nicht injiziert ist)
	local theme = ui and ui.theme or nil
	local wibar_theme = theme and theme.wibar or require("theme.wibar")
	local tabs_theme = (theme and theme.tabs and theme.tabs.get) and theme.tabs.get(cfg.tabs or {}) or nil
	local start_theme = (theme and theme.start and theme.start.get) and theme.start.get(cfg.start or {}) or nil
	local menu_theme = cfg.menus

	-- Widgets
	local tabs = Tabs.build(s, {
		modkey = modkey,
		group_by_class = true,
		theme = tabs_theme,
		menu_theme = menu_theme,
	})

	local tags = Tags.build(s, {})

	local tray = showtray and Systray.build({ menu_theme = menu_theme }) or nil
	local clock = Clock.build() -- liest clock-Keys aus beautiful.*

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

	local layoutbox = wibox.widget({
		s.mylayoutbox,
		left = beautiful.layoutbox_pad_h or 0,
		right = beautiful.layoutbox_pad_h or 0,
		top = beautiful.layoutbox_pad_v or 0,
		bottom = beautiful.layoutbox_pad_v or 0,
		widget = wibox.container.margin,
	})

	local start_btn = Start.build({
		screen = s,
		theme = start_theme,
		launcher = cfg.launcher,
		terminal = cfg.terminal,
		menu = cfg.mymainmenu,
	})

	-- Props + Layout aus dem Theme
	local props = (wibar_theme.props and wibar_theme.props())
		or {
			position = "bottom",
			height = 28,
			bg = beautiful.wibar_bg or beautiful.bg_normal,
			fg = beautiful.wibar_fg or beautiful.fg_normal,
		}

	local sections = (
		wibar_theme.layout
		and wibar_theme.layout(s, {
			start_btn = start_btn,
			tags = tags,
			tabs = tabs,
			tray = tray,
			clock = clock,
			layoutbox = layoutbox,
			spacing_l = 8,
			spacing_r = 0,
		})
	)
		or {
			left = {
				layout = wibox.layout.fixed.horizontal,
				start_btn,
				(tags and tags.indicator) or nil,
				(tabs and tabs.tasklist) or nil,
			},
			center = nil,
			right = { layout = wibox.layout.fixed.horizontal, layoutbox, tray, clock },
		}

	-- Wibar erstellen
	s.mywibar = awful.wibar({
		position = props.position or "bottom",
		screen = s,
		height = props.height,
		bg = props.bg,
		fg = props.fg,
		ontop = props.on_top,
		opacity = props.opacity,
		shape = props.shape,
		margins = props.margins,
	})

	s.mywibar:setup({
		layout = wibox.layout.align.horizontal,
		sections.left,
		sections.center,
		sections.right,
	})

	return s.mywibar
end

return M
