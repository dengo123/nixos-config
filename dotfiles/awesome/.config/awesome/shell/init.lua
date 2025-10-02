-- ~/.config/awesome/shell/init.lua
local awful = require("awful")

local M = {
	bar = require("shell.bar"),
	workspaces = require("shell.workspaces"),
	windowing = require("shell.windowing"),
	menu = require("shell.menu"), -- << neu
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

-- Erwartet { cfg, ui, input }
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

	-- 3) Menü initialisieren (stellt get_start_items & show_for_* bereit)
	M.menu.init({ ui = ui, cfg = cfg })

	-- 4) cfg-Launcher neutralisieren (steuerst du extern)
	cfg.mymainmenu = nil
	cfg.mylauncher = nil

	-- 5) Optionaler Launcher-Fn
	if type(cfg.launcher) == "string" and cfg.launcher:lower() ~= "launcher" and #cfg.launcher > 0 then
		local cmd = cfg.launcher
		cfg.launcher_fn = function()
			awful.spawn.with_shell(cmd)
		end
	else
		cfg.launcher_fn = nil
	end

	-- 6) Bars pro Screen – Menü-API durchreichen
	awful.screen.connect_for_each_screen(function(s)
		M.bar.setup(s, {
			cfg = cfg,
			ui = ui,
			menu_api = M.menu, -- << nur die API injizieren
		})
	end)

	return cfg
end

return M
