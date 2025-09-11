-- ~/.config/awesome/rc.lua
pcall(require, "luarocks.loader")

local gears = require("gears")
local awful = require("awful")
require("awful.autofocus")
local beautiful = require("beautiful")
local menubar = require("menubar")

-- Module-Pfade
local confdir = gears.filesystem.get_configuration_dir()
package.path = confdir .. "?.lua;" .. confdir .. "?/init.lua;" .. package.path

-- === zentrale Konfiguration & Core-Teile ===
local cfg = require("system.config") -- { terminal, editor, modkey, theme, ... }
local theme = require("ui.theme")
local layouts = require("ui.layouts")
local wallpaper = require("ui.wallpaper")

-- eigene Module
local mouse = require("input.mouse")
local kb = require("input.keys")
local tags = require("policy.tags")

-- statt widgets.bar + widgets.menu → features.shell
local shell = require("features.shell")

-- NEU: zentrales Windowing (ersetzt policy.rules + policy.signals)
local windowing = require("features.windowing")

require("system.errors").hook()

-- Theme + Layouts
theme.apply(cfg)
layouts:apply()

-- Menü/Launcher
local mw = shell.menu.create({ cfg = cfg, awesome_icon = beautiful.awesome_icon })
local mylauncher = mw.launcher
local mymainmenu = mw.menu
menubar.utils.terminal = cfg.terminal

-- Root-Maus
mouse.apply_root(mymainmenu)

-- Keyboard-Layout-Widget (WICHTIG: vor bar.setup definieren!)
local mykeyboardlayout = awful.widget.keyboardlayout()

-- Wallpaper-Signale
if wallpaper.hook then
	wallpaper.hook()
else
	screen.connect_signal("property::geometry", wallpaper.set)
end

-- Windowing initialisieren (Rules + Client-Signals + Titlebar)
windowing.init({
	modkey = cfg.modkey,
	mouse = mouse,
	client_opts = { sloppy_focus = true }, -- bei Bedarf auf false setzen
	titlebar_opts = { position = "top", size = 28 }, -- Taskbar/Titlebar-Layout
})

-- Genau EIN per-Screen-Block
awful.screen.connect_for_each_screen(function(s)
	wallpaper.set(s)
	tags.ensure(s)
	tags.renumber(s)

	shell.bar.setup(s, {
		cfg = cfg,
		launcher = mylauncher,
		keyboardlayout = mykeyboardlayout,
	})
end)

-- Keys (global)
kb.apply(cfg)
