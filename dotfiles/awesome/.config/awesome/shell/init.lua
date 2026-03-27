-- ~/.config/awesome/shell/init.lua
local awful = require("awful")

local Compat = require("lib.compat")
local Resolve = require("lib.resolve")

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
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function ctx()
	return runtime.ctx or {}
end

local function shell_ctx()
	local c = ctx()
	c.shell = c.shell or {}
	return c.shell
end

local function shell_mod(name)
	return shell_ctx()[name]
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

local function build_overlay_entry(name, api_obj, is_open_name, close_name)
	return {
		name = name,
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

	local Launchers = shell_mod("launchers")
	local Menu = shell_mod("menu")
	local Notify = shell_mod("notify")

	if Launchers and type(Launchers.build_overlays) == "function" then
		for _, entry in ipairs(Launchers.build_overlays()) do
			table.insert(ordered, entry)
		end
	end

	table.insert(ordered, build_overlay_entry("menu", Menu, "is_open", "hide"))
	table.insert(ordered, build_overlay_entry("notify_center", Notify, "is_center_open", "close_center"))

	return {
		ordered = ordered,
	}
end

local function build_actions()
	local Windowing = shell_mod("windowing")
	local Workspaces = shell_mod("workspaces")

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

local function ensure_compat_roots(c)
	c.cfg = c.cfg or {}
	c.ui = c.ui or {}
	c.input = c.input or {}
	c.shell = c.shell or {}
	c.features = c.features or {}
	c.services = c.services or {}
	c.policy = c.policy or {}
	c.external = c.external or {}
	c.api = c.api or {}
	c.cfg.api = c.cfg.api or {}
end

local function register_shell_module(name, value)
	if not name then
		return value
	end

	local c = ctx()
	ensure_compat_roots(c)

	c.shell[name] = value

	if name == "windowing" then
		c.features.windowing = value
	elseif name == "workspaces" then
		c.features.workspaces = value
	elseif name == "notify" then
		c.features.notify = value
	elseif name == "menu" then
		c.features.menu = value
	elseif name == "launchers" then
		c.features.launchers = value
	elseif name == "bar" then
		c.features.bar = value
	end

	c.cfg.api[name] = value
	c.api[name] = value

	return value
end

local function init_module(name)
	local c = ctx()
	local mod = shell_mod(name)

	if not (mod and type(mod.init) == "function") then
		return mod
	end

	local out = mod.init(c)
	if out ~= nil then
		register_shell_module(name, out)
		return out
	end

	return mod
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(args)
	runtime.ctx = args or {}

	local c = ctx()
	ensure_compat_roots(c)

	-- ---------------------------------------------------------------------
	-- Load modules
	-- ---------------------------------------------------------------------

	register_shell_module("bar", safe_require("shell.bar"))
	register_shell_module("workspaces", safe_require("shell.workspaces"))
	register_shell_module("windowing", safe_require("shell.windowing"))
	register_shell_module("launchers", safe_require("shell.launchers"))
	register_shell_module("menu", safe_require("shell.menu"))
	register_shell_module("notify", safe_require("shell.notify"))

	-- ---------------------------------------------------------------------
	-- Shared API / compatibility
	-- ---------------------------------------------------------------------

	c.cfg.api.ui = c.ui
	c.cfg.api.input = c.input

	local wallpaper_fn = resolve_wallpaper_fn(c.ui)
	if wallpaper_fn then
		c.cfg.wallpaper_fn = wallpaper_fn
	end

	Compat.apply(c)

	-- ---------------------------------------------------------------------
	-- Init submodules
	-- ---------------------------------------------------------------------

	init_module("workspaces")
	init_module("windowing")
	init_module("launchers")
	init_module("menu")
	init_module("notify")

	Compat.apply(c)

	-- ---------------------------------------------------------------------
	-- Runtime state
	-- ---------------------------------------------------------------------

	c.cfg.overlays = build_overlays()
	c.cfg.actions = build_actions()

	-- ---------------------------------------------------------------------
	-- Bar last
	-- ---------------------------------------------------------------------

	init_module("bar")

	Compat.apply(c)

	local Bar = shell_mod("bar")
	local Menu = shell_mod("menu")

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
