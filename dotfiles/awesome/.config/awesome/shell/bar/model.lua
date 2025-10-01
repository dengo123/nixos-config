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
	local ui = opts.ui -- kommt aus shell.init → M.bar.setup
	local modkey = cfg.modkey or "Mod4"
	local kb = opts.keyboardlayout
	local showtray = (opts.systray ~= false)

	-- === THEMES zentral aus ui.theme holen ===
	local theme = ui and ui.theme or nil

	-- Tabs-Theme (erforderlich für Tabs.build)
	local TabsTheme = nil
	if theme and theme.tabs and theme.tabs.get then
		TabsTheme = theme.tabs.get(cfg.tabs or {})
	end

	-- Optional: Menü-Theme (weiß/beige, schwarze Schrift), z. B. aus cfg.menus
	local MenuTheme = cfg.menus

	-- Weitere Teil-Themes könntest du bei Bedarf genauso ziehen:
	-- local TagsTheme  = (theme and theme.tags  and theme.tags.get)  and theme.tags.get(cfg.tags  or {})  or nil
	-- local WibarTheme = (theme and theme.wibar and theme.wibar.get) and theme.wibar.get(cfg.wibar or {}) or nil

	-- === Widgets aufbauen ===
	local tabs = Tabs.build(s, {
		modkey = modkey,
		group_by_class = true,
		theme = TabsTheme, -- << WICHTIG: Tabs-Theme reinreichen
		menu_theme = MenuTheme, -- << optional: zentrales Dropdown-Theme
	})

	local tags = Tags.build(s, { -- falls dein Tags-Widget eigene opts will, hier rein
		-- theme = TagsTheme,
	})

	local tray = showtray and Systray.build() or nil
	local clock = Clock.build("%H:%M")

	local start_btn = Start.build(s, {
		label = "Start",
		icon = cfg.start_icon or cfg.awesome_icon,
		menu = cfg.mymainmenu, -- kann nil sein
		on_press = cfg.launcher_fn, -- optionaler externer Launcher
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
		kb,
		tray,
		clock,
		s.mylayoutbox,
	}

	return { left = left, center = center, right = right }
end

return M
