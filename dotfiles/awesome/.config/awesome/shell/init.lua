-- ~/.config/awesome/shell/init.lua
local awful = require("awful")

local M = {
	bar = {
		model = require("shell.bar.model"),
		view = require("shell.bar.view"),
	},
	workspaces = require("shell.workspaces"),
	windowing = require("shell.windowing"),
}

-- Aus ui.* eine Wallpaper-Setter-Funktion extrahieren (falls vorhanden)
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

function M.bar.setup(s, opts)
	opts = opts or {}
	local model = M.bar.model.build(s, opts)
	return M.bar.view.place(s, model, opts)
end

-- Erwartet { cfg, ui, input }
function M.init(args)
	args = args or {}
	local cfg = args.cfg or {}
	local ui = args.ui
	local input = args.input or {}

	-- 1) Workspaces: komplette cfg durchreichen (+ optional wallpaper_fn)
	do
		local wcfg = {}
		for k, v in pairs(cfg) do
			wcfg[k] = v
		end
		-- Falls dein Workspaces-Setup noch eine Wallpaper-Funktion nutzen soll:
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

	-- 3) Kein internes Menü
	cfg.mymainmenu = nil
	cfg.mylauncher = nil

	-- 4) Launcher-Funktion (optional)
	if type(cfg.launcher) == "string" and cfg.launcher:lower() ~= "launcher" and #cfg.launcher > 0 then
		local cmd = cfg.launcher
		cfg.launcher_fn = function()
			awful.spawn.with_shell(cmd)
		end
	else
		cfg.launcher_fn = nil
	end

	-- 5) Bars pro Screen
	awful.screen.connect_for_each_screen(function(s)
		M.bar.setup(s, {
			cfg = cfg,
			ui = ui,
			launcher = nil,
		})
	end)

	return cfg
end

return M
