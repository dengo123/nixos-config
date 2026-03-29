-- ~/.config/awesome/input/init.lua
local gears = require("gears")

local function safe_require(path)
	local ok, mod = pcall(require, path)
	if ok then
		return mod
	end

	return nil
end

local M = {
	globalkeys = nil,
	global = {},
	client = {},
}

local runtime = {
	ctx = {},
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function ctx()
	return runtime.ctx or {}
end

local function cfg()
	return ctx().cfg or {}
end

local function ui()
	return ctx().ui or {}
end

local function shell()
	return ctx().shell or {}
end

local function init_module(mod, args)
	if type(mod) == "table" and type(mod.init) == "function" then
		mod.init(args)
	end
end

local function call_key_factory(mod, ...)
	if type(mod) == "function" then
		return mod(...)
	end

	if type(mod) == "table" and type(mod.build) == "function" then
		return mod.build(...)
	end

	return {}
end

local function launchers_open()
	local Launchers = shell().launchers
	return (Launchers and Launchers.input and Launchers.input.open) or {}
end

local function windowing_input()
	local Windowing = shell().windowing
	return (Windowing and Windowing.input) or {}
end

local function workspaces_input()
	local Workspaces = shell().workspaces
	return (Workspaces and Workspaces.input) or {}
end

local function screen_actions()
	return windowing_input().screens or {}
end

local function client_actions()
	return windowing_input().clients or {}
end

local function tag_actions()
	return workspaces_input().tags or {}
end

local function build_globalkeys()
	local conf = cfg()
	local modkey = conf.input and conf.input.modkey
	local join = gears.table.join

	local open = launchers_open()

	return join(
		call_key_factory(M.global.apps, modkey, conf),
		call_key_factory(M.global.awesome, modkey),
		call_key_factory(M.global.display, modkey, conf),
		call_key_factory(M.global.layout, modkey),
		call_key_factory(M.global.logoff, modkey, open),
		call_key_factory(M.global.media, modkey, conf),
		call_key_factory(M.global.menu, modkey, conf),
		call_key_factory(M.global.notify, modkey, conf),
		call_key_factory(M.global.power, modkey, open),
		call_key_factory(M.global.run, modkey, open),
		call_key_factory(M.global.screens, modkey, screen_actions()),
		call_key_factory(M.global.screenshot, modkey, conf),
		call_key_factory(M.global.tags, modkey, tag_actions())
	)
end

local function build_clientkeys()
	local conf = cfg()
	local modkey = conf.input and conf.input.modkey
	local join = gears.table.join

	local clients = client_actions()
	local screens = screen_actions()
	local tags = tag_actions()

	return join(
		call_key_factory(M.client.kill, modkey),
		call_key_factory(M.client.mouse, modkey),
		call_key_factory(M.client.navigation, modkey, clients),
		call_key_factory(M.client.screens, modkey, screens),
		call_key_factory(M.client.state, modkey, conf, clients),
		call_key_factory(M.client.tags, modkey, tags)
	)
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(args)
	runtime.ctx = args or {}

	M.global = {
		apps = safe_require("input.global.apps"),
		awesome = safe_require("input.global.awesome"),
		display = safe_require("input.global.display"),
		layout = safe_require("input.global.layout"),
		logoff = safe_require("input.global.logoff"),
		media = safe_require("input.global.media"),
		menu = safe_require("input.global.menu"),
		notify = safe_require("input.global.notify"),
		power = safe_require("input.global.power"),
		run = safe_require("input.global.run"),
		screens = safe_require("input.global.screens"),
		screenshot = safe_require("input.global.screenshot"),
		tags = safe_require("input.global.tags"),
	}

	M.client = {
		kill = safe_require("input.client.kill"),
		mouse = safe_require("input.client.mouse"),
		navigation = safe_require("input.client.navigation"),
		screens = safe_require("input.client.screens"),
		state = safe_require("input.client.state"),
		tags = safe_require("input.client.tags"),
	}

	init_module(M.client.mouse, {
		ui = ui(),
		cfg = cfg(),
		input = M,
		shell = shell(),
	})

	return M
end

function M.apply(_cfg)
	local globalkeys = build_globalkeys()
	local clientkeys = build_clientkeys()
	local rootkeys = gears.table.join(globalkeys, clientkeys)

	M.globalkeys = rootkeys
	root.keys(rootkeys)

	return M
end

function M.client_mouse()
	return M.client.mouse
end

return M
