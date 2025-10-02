-- ~/.config/awesome/shell/init.lua
local awful = require("awful")

local M = {
	bar = require("shell.bar"),
	workspaces = require("shell.workspaces"),
	windowing = require("shell.windowing"),
	launchers = require("shell.launchers"), -- << neu: zentrale Launcher-API
	menu = require("shell.menu"),
}

local function resolve_wallpaper_fn(ui)
	if not ui or not ui.wallpaper then
		return nil
	end
	if type(ui.wallpaper.set) == "function" then
		return ui.wallpaper.set
	end
	if type(ui.wallpaper) == "function" then
		return ui.wallpaper
	end
	return nil
end

-- args = { cfg, ui, input }
function M.init(args)
	args = args or {}
	local cfg = args.cfg or {}
	local ui = args.ui
	local input = args.input or {}

	-- 1) Workspaces
	do
		local wcfg = {}
		for k, v in pairs(cfg) do
			wcfg[k] = v
		end
		local wpfn = resolve_wallpaper_fn(ui)
		if wpfn then
			wcfg.wallpaper_fn = wpfn
		end
		M.workspaces.init(wcfg)
	end

	-- 2) Windowing
	M.windowing.init({
		modkey = cfg.modkey,
		mouse = input.mouse,
		ui = ui,
	})

	-- 3) Menü initialisieren + Launcher-API injizieren
	M.menu.init({
		ui = ui,
		cfg = cfg,
		launchers = M.launchers, -- << Menü öffnet Power/Run via dieser API
	})

	-- 3b) Launcher-API auch anderen Subsystemen (z. B. Keybinds) geben
	cfg.launchers = M.launchers
	-- Optional: Backcompat für alten Code, der dialogs erwartet:
	cfg.dialogs = M.launchers
	-- Optional global:
	-- rawset(_G, "__shell_launchers", M.launchers)

	-- 4) cfg-Launcher neutralisieren (steuerst du extern)
	cfg.mymainmenu = nil
	cfg.mylauncher = nil

	-- 5) Optionaler „legacy“ Launcher-Fn (Shell-Command)
	if type(cfg.launcher) == "string" and cfg.launcher:lower() ~= "launcher" and #cfg.launcher > 0 then
		local cmd = cfg.launcher
		cfg.launcher_fn = function()
			awful.spawn.with_shell(cmd)
		end
	else
		cfg.launcher_fn = nil
	end

	-- 6) Bars pro Screen – Menü-API injizieren
	awful.screen.connect_for_each_screen(function(s)
		M.bar.setup(s, {
			cfg = cfg,
			ui = ui,
			menu_api = M.menu,
		})
	end)

	return cfg
end

return M
