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
	-- Independent modules
	-- ---------------------------------------------------------------------

	init_child("launchers")

	-- ---------------------------------------------------------------------
	-- Menu / Notify
	-- ---------------------------------------------------------------------

	do
		local Menu = shell().menu
		if Menu and type(Menu.init) == "function" then
			local out = Menu.init({
				cfg = c.cfg,
				ui = c.ui,
				shell = shell(),
				launchers = shell().launchers,
			})

			if out ~= nil then
				shell().menu = out
			end
		end
	end

	do
		local Notify = shell().notify
		if Notify and type(Notify.init) == "function" then
			local out = Notify.init({
				cfg = c.cfg,
				ui = c.ui,
				input = c.input,
				system = c.system,
				modkey = c.modkey,
				mouse = c.mouse,
				shell = shell(),
			})

			if out ~= nil then
				shell().notify = out
			end
		end
	end

	-- ---------------------------------------------------------------------
	-- Bar last
	-- ---------------------------------------------------------------------

	init_child("bar")

	local Bar = shell().bar
	local Menu = shell().menu
	local Notify = shell().notify

	if Bar and type(Bar.setup) == "function" then
		awful.screen.connect_for_each_screen(function(s)
			Bar.setup(s, {
				cfg = c.cfg,
				ui = c.ui,
				menu = Menu,
				notify = Notify,
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
