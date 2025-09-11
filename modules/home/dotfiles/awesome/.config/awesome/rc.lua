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
local ui = require("ui") -- (theme + wallpaper bundle)
local layouts = require("features.workspaces.layouts") -- ⬅️ hier liegt dein layouts.lua

-- eigene Module
local input = require("input") -- (keys + mouse bundle)
local shell = require("features.shell")
local windowing = require("features.windowing")
local workspaces = require("features.workspaces")

require("system.errors").hook()

-- UI initialisieren (Theme zuerst, dann Wallpaper)
ui.init(cfg)

-- Layouts anwenden (aus features/workspaces/layouts.lua)
layouts:apply()

-- Menü/Launcher
local mw = shell.menu.create({ cfg = cfg, awesome_icon = beautiful.awesome_icon })
local mylauncher = mw.launcher
local mymainmenu = mw.menu
menubar.utils.terminal = cfg.terminal

-- Keyboard-Layout-Widget (WICHTIG: vor bar.setup definieren!)
local mykeyboardlayout = awful.widget.keyboardlayout()

-- Workspaces initialisieren (Tags, Layout-Policy, Wallpaper-Signale)
workspaces.init({
	wallpaper_fn = ui.wallpaper.set,
	ensure_one_tag = true,
	renumber_on_start = true,
	auto_adapt_layout_on_rotation = true,
})

-- Windowing initialisieren (Rules + Client-Signals + Titlebar)
windowing.init({
	modkey = cfg.modkey,
	mouse = input.mouse,
	client_opts = { sloppy_focus = true },
	titlebar_opts = { position = "top", size = 28 },
})

-- Genau EIN per-Screen-Block
awful.screen.connect_for_each_screen(function(s)
	-- ui.wallpaper.set(s) -- übernimmt schon workspaces.init()
	shell.bar.setup(s, {
		cfg = cfg,
		launcher = mylauncher,
		keyboardlayout = mykeyboardlayout,
	})
end)

-- Keys + Root-Mouse anwenden (zentral über input)
cfg.mymainmenu = mymainmenu
input.apply(cfg)
