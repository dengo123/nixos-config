-- ~/.config/awesome/rc.lua
pcall(require, "luarocks.loader")

local gears = require("gears")
local awful = require("awful")
require("awful.autofocus")

-- Module-Pfade
local confdir = gears.filesystem.get_configuration_dir()
package.path = confdir .. "?.lua;" .. confdir .. "?/init.lua;" .. package.path

-- === System (Config + Error-Hooks via system.init) ===
local system = require("system")
local cfg = system.init()

-- === UI (Theme + Wallpaper) ===
local ui = require("ui")
ui.init(cfg)

-- === eigene Module ===
local input = require("input")
local shell = require("features.shell")
local windowing = require("features.windowing")
local workspaces = require("features.workspaces")

-- Workspaces (Layouts, Wallpaper-Signale, Tags, Policy)
workspaces.init({
	wallpaper_fn = ui.wallpaper.set,
})

-- Windowing (Rules, Client-Signals, Titlebar)
windowing.init({
	modkey = cfg.modkey,
	mouse = input.mouse,
})

-- Shell (Menü + Bar pro Screen; setzt cfg.mymainmenu / cfg.mylauncher)
shell.init(cfg)

-- Globale Inputs (Keys + Root-Mouse; nutzt cfg.mymainmenu)
input.apply(cfg)
