-- ~/.config/awesome/shell/init.lua
local awful = require("awful")

local function safe_require(path)
	local ok, mod = pcall(require, path)
	if ok then
		return mod
	end

	return nil
end

local M = {
	api = {},
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function api()
	return M.api or {}
end

local function mod(name)
	return api()[name]
end

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

local function build_overlay_entry(api_obj, is_open_name, close_name)
	return {
		is_open = function()
			return api_obj and type(api_obj[is_open_name]) == "function" and api_obj[is_open_name]() or false
		end,
		close = function()
			if api_obj and type(api_obj[close_name]) == "function" then
				api_obj[close_name]()
			end
		end,
	}
end

local function build_overlays()
	local ordered = {}

	local Launchers = mod("launchers")
	local Menu = mod("menu")
	local Notify = mod("notify")

	if Launchers and type(Launchers.build_overlays) == "function" then
		for _, entry in ipairs(Launchers.build_overlays()) do
			table.insert(ordered, entry)
		end
	end

	table.insert(ordered, build_overlay_entry(Menu, "is_open", "hide"))
	table.insert(ordered, build_overlay_entry(Notify, "is_center_open", "close_center"))

	return {
		ordered = ordered,
	}
end

local function build_actions()
	local Windowing = mod("windowing")
	local Workspaces = mod("workspaces")

	local windowing_actions = (Windowing and Windowing.actions) or {}
	local workspace_actions = (Workspaces and Workspaces.actions) or {}

	return {
		windowing = {
			screens = {
				scr_in_dir = windowing_actions.scr_in_dir,
				move_client_to_screen = windowing_actions.move_client_to_screen,
			},
			clients = {
				move_client_dir = windowing_actions.move_client_dir,
				move_client_to_screen = windowing_actions.move_client_to_screen,
				layout_state_mode = windowing_actions.layout_state_mode,
				is_layout_state_active = windowing_actions.is_layout_state_active,
				toggle_layout_state = windowing_actions.toggle_layout_state,
			},
		},
		workspaces = {
			tags = {
				view_tag_idx = workspace_actions.view_tag_idx,
				move_tag_to_screen = workspace_actions.move_tag_to_screen,
				move_client_to_neighbor_tag = workspace_actions.move_client_to_neighbor_tag,
				add = Workspaces and Workspaces.add or nil,
				add_silent = Workspaces and Workspaces.add_silent or nil,
				delete_current = Workspaces and Workspaces.delete_current or nil,
				delete_current_force = Workspaces and Workspaces.delete_current_force or nil,
			},
		},
	}
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(args)
	args = args or {}

	local cfg = args.cfg or {}
	local ui = args.ui or {}
	local input = args.input or {}

	M.api = {
		ui = ui,
		input = input,
		bar = safe_require("shell.bar"),
		workspaces = safe_require("shell.workspaces"),
		windowing = safe_require("shell.windowing"),
		launchers = safe_require("shell.launchers"),
		menu = safe_require("shell.menu"),
		notify = safe_require("shell.notify"),
	}

	local Bar = mod("bar")
	local Workspaces = mod("workspaces")
	local Windowing = mod("windowing")
	local Launchers = mod("launchers")
	local Menu = mod("menu")
	local Notify = mod("notify")

	assert(Bar and type(Bar) == "table", "shell.init: shell.bar fehlt")
	assert(Workspaces and type(Workspaces) == "table", "shell.init: shell.workspaces fehlt")
	assert(Windowing and type(Windowing) == "table", "shell.init: shell.windowing fehlt")
	assert(Menu and type(Menu) == "table", "shell.init: shell.menu fehlt")
	assert(Notify and type(Notify) == "table", "shell.init: shell.notify fehlt")

	-- ---------------------------------------------------------------------
	-- Shared API
	-- ---------------------------------------------------------------------

	cfg.api = cfg.api or {}
	cfg.api.ui = ui
	cfg.api.input = input

	local wallpaper_fn = resolve_wallpaper_fn(ui)
	if wallpaper_fn then
		cfg.wallpaper_fn = wallpaper_fn
	end

	-- ---------------------------------------------------------------------
	-- Workspaces
	-- ---------------------------------------------------------------------

	assert(type(Workspaces.init) == "function", "shell.init: shell.workspaces.init fehlt")

	local workspace_api = Workspaces.init({
		cfg = cfg,
		ui = ui,
	})

	if workspace_api then
		M.api.workspaces = workspace_api
		Workspaces = workspace_api
	end

	cfg.api.workspaces = Workspaces

	-- ---------------------------------------------------------------------
	-- Windowing
	-- ---------------------------------------------------------------------

	assert(type(Windowing.init) == "function", "shell.init: shell.windowing.init fehlt")

	local windowing_api = Windowing.init({
		modkey = cfg.input and cfg.input.modkey,
		ui = ui,
		cfg = cfg,
	})

	if windowing_api then
		M.api.windowing = windowing_api
		Windowing = windowing_api
	end

	cfg.api.windowing = Windowing

	-- ---------------------------------------------------------------------
	-- Launchers
	-- ---------------------------------------------------------------------

	if Launchers and type(Launchers.init) == "function" then
		local launchers_api = Launchers.init({
			cfg = cfg,
			ui = ui,
		})

		if launchers_api then
			M.api.launchers = launchers_api
			Launchers = launchers_api
		end
	end

	cfg.api.launchers = Launchers

	-- ---------------------------------------------------------------------
	-- Menu
	-- ---------------------------------------------------------------------

	assert(type(Menu.init) == "function", "shell.init: shell.menu.init fehlt")

	local menu_api = Menu.init({
		ui = ui,
		cfg = cfg,
	})

	if menu_api then
		M.api.menu = menu_api
		Menu = menu_api
	end

	cfg.api.menu = Menu

	-- ---------------------------------------------------------------------
	-- Notifications
	-- ---------------------------------------------------------------------

	assert(type(Notify.init) == "function", "shell.init: shell.notify.init fehlt")

	local notify_api = Notify.init({
		ui = ui,
		cfg = cfg,
	})

	if notify_api then
		M.api.notify = notify_api
		Notify = notify_api
	end

	cfg.api.notify = Notify

	-- ---------------------------------------------------------------------
	-- Runtime State
	-- ---------------------------------------------------------------------

	cfg.overlays = build_overlays()
	cfg.actions = build_actions()

	-- ---------------------------------------------------------------------
	-- Bars
	-- ---------------------------------------------------------------------

	assert(type(Bar.init) == "function", "shell.init: shell.bar.init fehlt")
	assert(type(Bar.setup) == "function", "shell.init: shell.bar.setup fehlt")

	local bar_api = Bar.init({
		ui = ui,
		cfg = cfg,
	})

	if bar_api then
		M.api.bar = bar_api
		Bar = bar_api
	end

	cfg.api.bar = Bar

	awful.screen.connect_for_each_screen(function(s)
		Bar.setup(s, {
			cfg = cfg,
			ui = ui,
			menu_api = Menu,
		})
	end)

	return cfg
end

return M
