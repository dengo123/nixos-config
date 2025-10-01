-- ~/.config/awesome/shell/bar/model.lua
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

	-- Start-Button-Theme
	local StartTheme = nil
	if theme and theme.start and theme.start.get then
		StartTheme = theme.start.get(cfg.start or {})
	end

	-- Optional: Menü-Theme (weiß/beige, schwarze Schrift), z. B. aus cfg.menus
	local MenuTheme = cfg.menus

	-- === Widgets aufbauen ===
	local tabs = Tabs.build(s, {
		modkey = modkey,
		group_by_class = true,
		theme = TabsTheme, -- << WICHTIG: Tabs-Theme reinreichen
		menu_theme = MenuTheme, -- << optional: zentrales Dropdown-Theme
	})

	local tags = Tags.build(s, {
		-- falls dein Tags-Widget eigene opts will, hier rein (z. B. theme = ...)
	})

	local tray = showtray
			and Systray.build({
				-- eigene (themed) Tray-Rechtsklick-Menüs:
				menu_theme = MenuTheme,
				-- pad_h / base_size / bg etc. kannst du bei Bedarf hier setzen
			})
		or nil

	local clock = Clock.build("%H:%M")

	-- Start-Button: Funktion (Launcher) + Optik (Theme) sauber getrennt
	local start_btn = Start.build({
		screen = s,
		theme = StartTheme, -- Optik aus ui/theme/start.lua:get(cfg.start)
		launcher = cfg.launcher, -- "rofi" | "emacs" | "awesome" | custom cmd | nil
		terminal = cfg.terminal, -- Fallback, wenn launcher leer/nil
		menu = cfg.mymainmenu, -- nötig, wenn launcher == "awesome"
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
		spacing = 0,
		kb,
		tray,
		clock,
		s.mylayoutbox,
	}

	return { left = left, center = center, right = right }
end

return M
