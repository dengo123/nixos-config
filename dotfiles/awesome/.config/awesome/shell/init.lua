local awful = require("awful")

local M = {
	bar = require("shell.bar"),
	workspaces = require("shell.workspaces"),
	windowing = require("shell.windowing"),
	launchers = require("shell.launchers"),
	menu = require("shell.menu"),
	notify = require("shell.notify"),
}

-- ============================================================================
-- Helpers
-- ============================================================================

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

-- ============================================================================
-- Init
-- ============================================================================

function M.init(args)
	args = args or {}
	local cfg = args.cfg or {}
	local ui = args.ui
	local input = args.input or {}

	-- ------------------------------------------------------------------------
	-- Workspaces
	-- ------------------------------------------------------------------------

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

	-- ------------------------------------------------------------------------
	-- Windowing
	-- ------------------------------------------------------------------------

	M.windowing.init({
		modkey = cfg.system.modkey,
		mouse = input.mouse,
		ui = ui,
		cfg = cfg,
	})

	-- ------------------------------------------------------------------------
	-- Menu
	-- ------------------------------------------------------------------------

	M.menu.init({
		ui = ui,
		cfg = cfg,
		launchers = M.launchers,
	})

	cfg.launchers = M.launchers
	cfg.dialogs = M.launchers

	-- ------------------------------------------------------------------------
	-- Notifications
	-- ------------------------------------------------------------------------

	M.notify.init(cfg)

	-- ------------------------------------------------------------------------
	-- Launcher State
	-- ------------------------------------------------------------------------

	cfg.mymainmenu = nil
	cfg.mylauncher = nil

	if
		type(cfg.system.launcher) == "string"
		and cfg.system.launcher:lower() ~= "launcher"
		and #cfg.system.launcher > 0
	then
		local cmd = cfg.system.launcher
		cfg.launcher_fn = function()
			awful.spawn.with_shell(cmd)
		end
	else
		cfg.launcher_fn = nil
	end

	-- ------------------------------------------------------------------------
	-- Bars
	-- ------------------------------------------------------------------------

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
