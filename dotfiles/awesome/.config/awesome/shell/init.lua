-- ~/.config/awesome/shell/init.lua
local awful = require("awful")

local function safe_require(path)
	local ok, mod = pcall(require, path)
	if ok then
		return mod
	end

	return nil
end

local M = {}

local runtime = {
	ctx = {},
	shell = {},
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function ctx()
	return runtime.ctx or {}
end

local function shell()
	return runtime.shell or {}
end

local function ensure_root_ctx(c)
	c.cfg = c.cfg or {}
	c.ui = c.ui or {}
	c.input = c.input or {}
	c.system = c.system or {}
	return c
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

local function init_child(name)
	local mod = shell()[name]
	if not (mod and type(mod.init) == "function") then
		return mod
	end

	local c = ctx()
	local out = mod.init({
		cfg = c.cfg,
		ui = c.ui,
		input = c.input,
		system = c.system,
		modkey = c.modkey,
		mouse = c.mouse,
		shell = shell(),
	})

	if out ~= nil then
		shell()[name] = out
		return out
	end

	return mod
end

local function load_children()
	local s = shell()

	s.bar = safe_require("shell.bar")
	s.workspaces = safe_require("shell.workspaces")
	s.windowing = safe_require("shell.windowing")
	s.launchers = safe_require("shell.launchers")
	s.menu = safe_require("shell.menu")
	s.notify = safe_require("shell.notify")
end

local function build_actions()
	local Windowing = shell().windowing
	local Workspaces = shell().workspaces

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
				focus_client = windowing_actions.focus_client,
				swap_client = windowing_actions.swap_client,
				is_max_layout = windowing_actions.is_max_layout,
				current_screen = windowing_actions.current_screen,
				minimize_focused = windowing_actions.minimize_focused,
				minimize_visible_tag_on_screen = windowing_actions.minimize_visible_tag_on_screen,
				minimize_all_tags_on_screen = windowing_actions.minimize_all_tags_on_screen,
				minimize_visible_tags_on_all_screens = windowing_actions.minimize_visible_tags_on_all_screens,
				minimize_all_tags_on_all_screens = windowing_actions.minimize_all_tags_on_all_screens,
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
	runtime.ctx = ensure_root_ctx(args or {})
	runtime.shell = {}

	local c = ctx()
	c.shell = runtime.shell

	load_children()

	local wallpaper_fn = resolve_wallpaper_fn(c.ui)
	if wallpaper_fn then
		c.cfg.wallpaper_fn = wallpaper_fn
	end

	-- ---------------------------------------------------------------------
	-- Core shell trees first
	-- ---------------------------------------------------------------------

	init_child("windowing")
	init_child("workspaces")

	-- ---------------------------------------------------------------------
	-- Other shell modules
	-- ---------------------------------------------------------------------

	init_child("launchers")
	init_child("menu")
	init_child("notify")

	-- ---------------------------------------------------------------------
	-- Shared actions for input / config consumers
	-- ---------------------------------------------------------------------

	c.cfg.actions = build_actions()

	-- ---------------------------------------------------------------------
	-- Bar last
	-- ---------------------------------------------------------------------

	init_child("bar")

	local Bar = shell().bar
	local Menu = shell().menu

	if Bar and type(Bar.setup) == "function" then
		awful.screen.connect_for_each_screen(function(s)
			Bar.setup(s, {
				cfg = c.cfg,
				ui = c.ui,
				menu_api = Menu,
			})
		end)
	end

	if Bar and type(Bar.resync_all) == "function" then
		awesome.connect_signal("autorandr::applied", function()
			Bar.resync_all()
		end)
	end

	return c.shell
end

return M
