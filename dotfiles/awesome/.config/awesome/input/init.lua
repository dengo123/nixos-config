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

local function ensure_ctx_roots()
	local c = ctx()

	c.input = c.input or M
	c.api = c.api or {}
	c.external = c.external or {}
	c.cfg = c.cfg or {}
	c.cfg.api = c.cfg.api or {}
end

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

local function build_globalkeys()
	local conf = cfg()
	local modkey = conf.input and conf.input.modkey
	local join = gears.table.join

	return join(
		call_key_factory(global_mod("apps"), modkey, conf),
		call_key_factory(global_mod("awesome"), modkey),
		call_key_factory(global_mod("display"), modkey, conf),
		call_key_factory(global_mod("layout"), modkey),
		call_key_factory(global_mod("logoff"), modkey, conf.api and conf.api.launchers),
		call_key_factory(global_mod("media"), modkey, conf),
		call_key_factory(global_mod("menu"), modkey, conf),
		call_key_factory(global_mod("notify"), modkey, conf),
		call_key_factory(global_mod("power"), modkey, conf.api and conf.api.launchers),
		call_key_factory(global_mod("run"), modkey, conf.api and conf.api.launchers),
		call_key_factory(
			global_mod("screens"),
			modkey,
			conf.actions and conf.actions.windowing and conf.actions.windowing.screens
		),
		call_key_factory(global_mod("screenshot"), modkey, conf),
		call_key_factory(
			global_mod("tags"),
			modkey,
			conf.actions and conf.actions.workspaces and conf.actions.workspaces.tags
		)
	)
end

local function build_clientkeys()
	local conf = cfg()
	local modkey = conf.input and conf.input.modkey
	local join = gears.table.join

	return join(
		call_key_factory(client_mod("kill"), modkey),
		call_key_factory(client_mod("mouse"), modkey),
		call_key_factory(
			client_mod("navigation"),
			modkey,
			conf.actions and conf.actions.windowing and conf.actions.windowing.clients
		),
		call_key_factory(
			client_mod("screens"),
			modkey,
			conf.actions and conf.actions.windowing and conf.actions.windowing.screens
		),
		call_key_factory(client_mod("state"), modkey, conf),
		call_key_factory(
			client_mod("tags"),
			modkey,
			conf.actions and conf.actions.workspaces and conf.actions.workspaces.tags
		)
	)
end

local function init_runtimekeys(rootkeys)
	local conf = cfg()
	local Runtime = runtime_api()

	init_module(Runtime.escape, {
		globalkeys = rootkeys,
		overlays = conf.overlays,
	})

	init_module(Runtime.notify, {
		globalkeys = rootkeys,
		overlays = conf.overlays,
	})
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(args)
	runtime.ctx = args or {}
	ensure_ctx_roots()

	M.api = {
		ui = ui(),
		global = {
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
		},
		runtime = {
			escape = safe_require("input.runtime.escape"),
			notify = safe_require("input.runtime.notify"),
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

	-- ---------------------------------------------------------------------
	-- Compatibility mirrors
	-- ---------------------------------------------------------------------

	local c = ctx()
	c.input = M
	c.api.input = M
	c.external.input = M
	c.cfg.api.input = M

	-- ---------------------------------------------------------------------
	-- Client mouse init
	-- ---------------------------------------------------------------------

	init_module(M.client.mouse, {
		ctx = c,
		ui = ui(),
		cfg = cfg(),
		input = M,
		api = {
			ui = M.api.ui or {},
			input = M,
			cfg = cfg(),
		},
	})

	return M
end

function M.apply(_cfg)
	local globalkeys = build_globalkeys()
	local clientkeys = build_clientkeys()
	local rootkeys = gears.table.join(globalkeys, clientkeys)

	M.globalkeys = rootkeys
	root.keys(rootkeys)

	init_runtimekeys(rootkeys)

	return M
end

function M.client_mouse()
	return client_mod("mouse")
end

return M
