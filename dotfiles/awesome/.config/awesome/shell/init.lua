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
	bar = safe_require("shell.bar"),
	workspaces = safe_require("shell.workspaces"),
	windowing = safe_require("shell.windowing"),
	launchers = safe_require("shell.launchers"),
	menu = safe_require("shell.menu"),
	notify = safe_require("shell.notify"),
}

assert(M.bar and type(M.bar) == "table", "shell.init: shell.bar fehlt")
assert(M.workspaces and type(M.workspaces) == "table", "shell.init: shell.workspaces fehlt")
assert(M.windowing and type(M.windowing) == "table", "shell.init: shell.windowing fehlt")
assert(M.menu and type(M.menu) == "table", "shell.init: shell.menu fehlt")
assert(M.notify and type(M.notify) == "table", "shell.init: shell.notify fehlt")

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

	if M.launchers and type(M.launchers.build_overlays) == "function" then
		for _, entry in ipairs(M.launchers.build_overlays()) do
			table.insert(ordered, entry)
		end
	end

	table.insert(ordered, build_overlay_entry(M.menu, "is_open", "hide"))
	table.insert(ordered, build_overlay_entry(M.notify, "is_center_open", "close_center"))

	return {
		ordered = ordered,
	}
end

local function build_actions()
	local windowing_actions = (M.windowing and M.windowing.actions) or {}
	local workspace_api = M.workspaces or {}
	local workspace_actions = workspace_api.actions or {}

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
				add = workspace_api.add,
				add_silent = workspace_api.add_silent,
				delete_current = workspace_api.delete_current,
				delete_current_force = workspace_api.delete_current_force,
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
		local wallpaper_fn = resolve_wallpaper_fn(ui)

		if wallpaper_fn then
			cfg.wallpaper_fn = wallpaper_fn
		end

		assert(type(M.workspaces.init) == "function", "shell.init: shell.workspaces.init fehlt")

		local workspace_api = M.workspaces.init(cfg)

		if workspace_api then
			M.workspaces = workspace_api
		end
	end

	-- ---------------------------------------------------------------------
	-- Windowing
	-- ---------------------------------------------------------------------

	assert(type(M.windowing.init) == "function", "shell.init: shell.windowing.init fehlt")

	local windowing_api = M.windowing.init({
		modkey = cfg.input and cfg.input.modkey,
		mouse = input.mouse,
		ui = ui,
		cfg = cfg,
	})

	if windowing_api then
		M.windowing = windowing_api
	end

	-- ---------------------------------------------------------------------
	-- Launchers
	-- ---------------------------------------------------------------------

	if M.launchers and type(M.launchers.init) == "function" then
		M.launchers.init(cfg)
	end

	-- ---------------------------------------------------------------------
	-- Menu
	-- ---------------------------------------------------------------------

	assert(type(M.menu.init) == "function", "shell.init: shell.menu.init fehlt")

	M.menu.init({
		ui = ui,
		cfg = cfg,
	})

	-- ---------------------------------------------------------------------
	-- Notifications
	-- ---------------------------------------------------------------------

	assert(type(M.notify.init) == "function", "shell.init: shell.notify.init fehlt")

	M.notify.init(cfg)

	-- ---------------------------------------------------------------------
	-- Runtime State
	-- ---------------------------------------------------------------------

	cfg.overlays = build_overlays()
	cfg.actions = build_actions()

	cfg.api = {
		launchers = M.launchers,
		menu = M.menu,
		notify = M.notify,
		workspaces = M.workspaces,
		windowing = M.windowing,
	}

	-- ---------------------------------------------------------------------
	-- Bars
	-- ---------------------------------------------------------------------

	assert(type(M.bar.setup) == "function", "shell.init: shell.bar.setup fehlt")

	awful.screen.connect_for_each_screen(function(s)
		M.bar.setup(s, {
			cfg = cfg,
			menu_api = M.menu,
		})
	end)

	return cfg
end

return M
