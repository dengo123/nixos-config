-- shell/init.lua
local awful = require("awful")

local M = {
	bar = require("shell.bar"),
	workspaces = require("shell.workspaces"),
	windowing = require("shell.windowing"),
	launchers = require("shell.launchers"),
	menu = require("shell.menu"),
	notify = require("shell.notify"),
}

-- =========================================================================
-- Helpers
-- =========================================================================

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

local function build_overlay_entry(api, is_open_name, close_name)
	return {
		is_open = function()
			return api and type(api[is_open_name]) == "function" and api[is_open_name]() or false
		end,
		close = function()
			if api and type(api[close_name]) == "function" then
				api[close_name]()
			end
		end,
	}
end

local function build_overlays()
	local ordered = {}

	if M.launchers and M.launchers.power then
		table.insert(ordered, build_overlay_entry(M.launchers.power, "is_open", "close"))
	end

	if M.launchers and M.launchers.run then
		table.insert(ordered, build_overlay_entry(M.launchers.run, "is_open", "close"))
	end

	table.insert(ordered, build_overlay_entry(M.menu, "is_open", "close"))
	table.insert(ordered, build_overlay_entry(M.notify, "is_center_open", "close_center"))

	return {
		ordered = ordered,
	}
end

local function build_actions()
	local win = M.windowing.actions or {}
	local ws = M.workspaces

	return {
		windowing = {
			screens = {
				scr_in_dir = win.scr_in_dir,
				move_client_to_screen = win.move_client_to_screen,
			},
			clients = {
				move_client_dir = win.move_client_dir,
				move_client_to_screen = win.move_client_to_screen,
				move_client_to_neighbor_tag = win.move_client_to_neighbor_tag,
				toggle_pseudo_maximize = win.toggle_pseudo_maximize,
			},
		},
		workspaces = {
			tags = {
				view_tag_idx = win.view_tag_idx,
				move_tag_to_screen = win.move_tag_to_screen,
				add = ws.add,
				add_silent = ws.add_silent,
				delete_current = ws.delete_current,
				delete_current_force = ws.delete_current_force,
			},
		},
	}
end

-- =========================================================================
-- Init
-- =========================================================================

function M.init(args)
	args = args or {}

	local cfg = args.cfg or {}
	local ui = args.ui
	local input = args.input or {}

	-- ---------------------------------------------------------------------
	-- Workspaces
	-- ---------------------------------------------------------------------

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

	-- ---------------------------------------------------------------------
	-- Windowing
	-- ---------------------------------------------------------------------

	M.windowing.init({
		modkey = cfg.input.modkey,
		mouse = input.mouse,
		ui = ui,
		cfg = cfg,
	})

	-- ---------------------------------------------------------------------
	-- Menu
	-- ---------------------------------------------------------------------

	M.menu.init({
		ui = ui,
		cfg = cfg,
		launchers = M.launchers,
	})

	-- ---------------------------------------------------------------------
	-- Notifications
	-- ---------------------------------------------------------------------

	M.notify.init(cfg)

	-- ---------------------------------------------------------------------
	-- Runtime State
	-- ---------------------------------------------------------------------

	cfg.launchers = M.launchers
	cfg.overlays = build_overlays()
	cfg.actions = build_actions()

	-- ---------------------------------------------------------------------
	-- External Launcher
	-- ---------------------------------------------------------------------

	if type(cfg.apps.launcher) == "string" and cfg.apps.launcher:lower() ~= "launcher" and #cfg.apps.launcher > 0 then
		local cmd = cfg.apps.launcher

		cfg.launcher_fn = function()
			awful.spawn.with_shell(cmd)
		end
	else
		cfg.launcher_fn = nil
	end

	-- ---------------------------------------------------------------------
	-- Bars
	-- ---------------------------------------------------------------------

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
