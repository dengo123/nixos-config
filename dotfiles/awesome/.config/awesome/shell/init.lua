-- ~/.config/awesome/shell/init.lua
local awful = require("awful")

local M = {
	-- KEIN menu-Require mehr
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

-- High-Level-Init: Workspaces → Windowing → Notify → Bar (OHNE Menü)
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

	-- 3) Notifier früh initialisieren
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

	-- 4) KEIN internes Menü, KEIN Launcher-Widget
	cfg.mymainmenu = nil
	cfg.mylauncher = nil

	-- Optional: Externen Launcher als Funktion verfügbar machen (für Keybinds)
	-- Beispiel in system.config.launcher: "rofi -show drun" oder "emacsclient -c"
	if type(cfg.launcher) == "string" and cfg.launcher:lower() ~= "launcher" and #cfg.launcher > 0 then
		local cmd = cfg.launcher
		cfg.launcher_fn = function()
			awful.spawn.with_shell(cmd)
		end
	else
		cfg.launcher_fn = nil
	end

	-- 5) Bars pro Screen (Keyboardlayout-Widget einmalig)
	local mykeyboardlayout = awful.widget.keyboardlayout()
	awful.screen.connect_for_each_screen(function(s)
		M.bar.setup(s, {
			cfg = cfg,
			launcher = nil, -- explizit kein Launcher-Widget
			keyboardlayout = mykeyboardlayout,
		})
	end)

	return cfg
end

return M
