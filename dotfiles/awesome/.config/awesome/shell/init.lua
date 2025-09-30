-- ~/.config/awesome/shell/init.lua
local awful = require("awful")

local M = {
	menu = require("shell.menu"),
	bar = {
		model = require("shell.bar.model"),
		view = require("shell.bar.view"),
	},
	workspaces = require("shell.workspaces"),
	windowing = require("shell.windowing"),
	notify = require("shell.notify"),
}

-- interne Helper
local function resolve_wallpaper_fn(ui)
	if not ui or not ui.wallpaper then
		return nil
	end
	if type(ui.wallpaper.set) == "function" then
		return ui.wallpaper.set
	elseif type(ui.wallpaper) == "function" then
		return ui.wallpaper
	end
	return nil
end

-- Wrapper: Bar-Setup (Screen → Model → View)
function M.bar.setup(s, opts)
	local model = M.bar.model.build(s, opts or {})
	return M.bar.view.place(s, model, opts or {})
end

-- High-Level-Init: Workspaces → Windowing → Notify → Menu/Bar
-- Erwartet { cfg, ui, input }
function M.init(args)
	args = args or {}
	local cfg = args.cfg or {}
	local ui = args.ui
	local input = args.input or {}

	-- 1) Workspaces (Wallpaper-Hook aus UI weiterreichen)
	M.workspaces.init({
		wallpaper_fn = resolve_wallpaper_fn(ui),
	})

	-- 2) Windowing (Rules, Client-Signals, Titlebar) – braucht modkey + mouse
	M.windowing.init({
		modkey = cfg.modkey,
		mouse = input.mouse,
	})

	-- 3) Notifier früh initialisieren (vor Menu/Bar)
	M.notify.init({
		speech = true, -- true = Sprechblase, false = runde Ecken
		position = "bottom_right",
		bg = "#F5E6B3",
		fg = "#000000",
		border = "#C8B27A",
		radius = 8,
		icon_size = 24,
		timeout = 3,
	})

	-- 4) Menü bauen → API + Launcher ins cfg zurückgeben
	local mw = M.menu.setup(cfg) -- { menu = api, launcher = widget }
	cfg.mymainmenu = mw.menu
	cfg.mylauncher = mw.launcher

	-- 5) Bars pro Screen (Keyboardlayout-Widget einmalig)
	local mykeyboardlayout = awful.widget.keyboardlayout()
	awful.screen.connect_for_each_screen(function(s)
		M.bar.setup(s, {
			cfg = cfg,
			launcher = cfg.mylauncher,
			keyboardlayout = mykeyboardlayout,
		})
	end)

	-- cfg zurückschreiben (wichtig für input.apply in system.init)
	return cfg
end

return M
