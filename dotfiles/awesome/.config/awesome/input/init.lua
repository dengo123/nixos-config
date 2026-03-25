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
	api = {},
	globalkeys = nil,
	global = {},
	runtime = {},
	client = {},
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function api()
	return M.api or {}
end

local function group_api(name)
	return api()[name] or {}
end

local function global_api()
	return group_api("global")
end

local function runtime_api()
	return group_api("runtime")
end

local function client_api()
	return group_api("client")
end

local function global_mod(name)
	return global_api()[name]
end

local function runtime_mod(name)
	return runtime_api()[name]
end

local function client_mod(name)
	return client_api()[name]
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

local function init_module(mod, args)
	if type(mod) == "table" and type(mod.init) == "function" then
		mod.init(args)
	end
end

local function build_globalkeys(cfg)
	local modkey = cfg.input and cfg.input.modkey
	local join = gears.table.join

	return join(
		call_key_factory(global_mod("apps"), modkey, cfg),
		call_key_factory(global_mod("awesome"), modkey),
		call_key_factory(global_mod("display"), modkey, cfg),
		call_key_factory(global_mod("layout"), modkey),
		call_key_factory(global_mod("logoff"), modkey, cfg.api and cfg.api.launchers),
		call_key_factory(global_mod("media"), modkey, cfg),
		call_key_factory(global_mod("menu"), modkey, cfg),
		call_key_factory(global_mod("power"), modkey, cfg.api and cfg.api.launchers),
		call_key_factory(global_mod("run"), modkey, cfg.api and cfg.api.launchers),
		call_key_factory(
			global_mod("screens"),
			modkey,
			cfg.actions and cfg.actions.windowing and cfg.actions.windowing.screens
		),
		call_key_factory(global_mod("screenshot"), modkey, cfg),
		call_key_factory(
			global_mod("tags"),
			modkey,
			cfg.actions and cfg.actions.workspaces and cfg.actions.workspaces.tags
		)
	)
end

local function build_clientkeys(cfg)
	local modkey = cfg.input and cfg.input.modkey
	local join = gears.table.join

	return join(
		call_key_factory(client_mod("kill"), modkey),
		call_key_factory(client_mod("mouse"), modkey),
		call_key_factory(client_mod("navigation"), modkey),
		call_key_factory(
			client_mod("screens"),
			modkey,
			cfg.actions and cfg.actions.windowing and cfg.actions.windowing.screens
		),
		call_key_factory(client_mod("state"), modkey, cfg),
		call_key_factory(
			client_mod("tags"),
			modkey,
			cfg.actions and cfg.actions.workspaces and cfg.actions.workspaces.tags
		)
	)
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(args)
	args = args or {}

	M.api = {
		ui = args.ui or {},
		global = {
			apps = safe_require("input.global.apps"),
			awesome = safe_require("input.global.awesome"),
			display = safe_require("input.global.display"),
			layout = safe_require("input.global.layout"),
			logoff = safe_require("input.global.logoff"),
			media = safe_require("input.global.media"),
			menu = safe_require("input.global.menu"),
			power = safe_require("input.global.power"),
			run = safe_require("input.global.run"),
			screens = safe_require("input.global.screens"),
			screenshot = safe_require("input.global.screenshot"),
			tags = safe_require("input.global.tags"),
		},
		runtime = {
			escape = safe_require("input.runtime.escape"),
			super_release = safe_require("input.runtime.super_release"),
		},
		client = {
			kill = safe_require("input.client.kill"),
			mouse = safe_require("input.client.mouse"),
			navigation = safe_require("input.client.navigation"),
			screens = safe_require("input.client.screens"),
			state = safe_require("input.client.state"),
			tags = safe_require("input.client.tags"),
		},
	}

	M.global = M.api.global or {}
	M.runtime = M.api.runtime or {}
	M.client = M.api.client or {}

	init_module(M.client.mouse, {
		api = {
			ui = M.api.ui or {},
		},
	})

	return M
end

function M.apply(cfg)
	cfg = cfg or {}

	local Escape = runtime_mod("escape")

	local globalkeys = build_globalkeys(cfg)
	local clientkeys = build_clientkeys(cfg)
	local rootkeys = gears.table.join(globalkeys, clientkeys)

	M.globalkeys = rootkeys
	root.keys(rootkeys)

	if Escape and type(Escape.init) == "function" then
		Escape.init({
			globalkeys = rootkeys,
			overlays = cfg.overlays,
		})
	end

	return M
end

function M.client_mouse()
	return client_mod("mouse")
end

return M
