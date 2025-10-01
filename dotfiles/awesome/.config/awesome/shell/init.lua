-- shell/init.lua
local awful = require("awful")

local M = {
	bar = {
		model = require("shell.bar.model"),
		view = require("shell.bar.view"),
	},
	workspaces = require("shell.workspaces"),
	windowing = require("shell.windowing"),
}

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
	local model = M.bar.model.build(s, opts or {})
	return M.bar.view.place(s, model, opts or {})
end

-- Erwartet { cfg, ui, input }
function M.init(args)
	args = args or {}
	local cfg = args.cfg or {}
	local ui = args.ui
	local input = args.input or {}

	-- 1) Workspaces
	M.workspaces.init({
		wallpaper_fn = resolve_wallpaper_fn(ui),
	})

	-- 2) Windowing
	M.windowing.init({
		modkey = cfg.modkey,
		mouse = input.mouse,
		ui = ui,
	})

	-- 3) Kein internes Menü
	cfg.mymainmenu = nil
	cfg.mylauncher = nil

	if type(cfg.launcher) == "string" and cfg.launcher:lower() ~= "launcher" and #cfg.launcher > 0 then
		local cmd = cfg.launcher
		cfg.launcher_fn = function()
			awful.spawn.with_shell(cmd)
		end
	else
		cfg.launcher_fn = nil
	end

	-- 5) Bars pro Screen
	local mykeyboardlayout = awful.widget.keyboardlayout()
	awful.screen.connect_for_each_screen(function(s)
		M.bar.setup(s, {
			cfg = cfg,
			ui = ui, -- << NEU: UI (inkl. ui.theme) weitergeben
			launcher = nil,
			keyboardlayout = mykeyboardlayout,
		})
	end)

	return cfg
end

return M
