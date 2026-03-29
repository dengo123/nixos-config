-- ~/.config/awesome/<path>/module_skeleton.lua
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

local runtime = {
	ctx = {},
	state = {},
	initialized = false,
	signals_ready = false,
}

-- =========================================================================
-- Helpers: Context
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

local function input()
	return ctx().input or {}
end

local function system()
	return ctx().system or {}
end

-- =========================================================================
-- Helpers: Lifecycle
-- =========================================================================

local function is_initialized()
	return runtime.initialized == true
end

local function set_initialized(v)
	runtime.initialized = (v == true)
end

local function signals_ready()
	return runtime.signals_ready == true
end

local function set_signals_ready(v)
	runtime.signals_ready = (v == true)
end

-- =========================================================================
-- Helpers: Module utilities
-- =========================================================================

local function init_module(mod, args)
	if mod and type(mod.init) == "function" then
		return mod.init(args)
	end

	return mod
end

local function apply_module(mod, args)
	if mod and type(mod.apply) == "function" then
		return mod.apply(args)
	end

	return mod
end

local function mod(name)
	return M.api[name]
end

local function state()
	return runtime.state or {}
end

local function reset_state()
	runtime.state = {}
	return runtime.state
end

-- =========================================================================
-- Build public branches
-- =========================================================================

local function build_public_api()
	return {
		-- Beispiel:
		-- do_thing = function(...)
		-- 	return M.do_thing(...)
		-- end,
	}
end

local function build_input_branch()
	return {
		-- Beispiel:
		-- action = function(...)
		-- 	return M.do_thing(...)
		-- end,
	}
end

-- =========================================================================
-- Internal wiring
-- =========================================================================

local function register_signals()
	if signals_ready() then
		return
	end

	set_signals_ready(true)

	-- Beispiel:
	-- awesome.connect_signal("module::refresh", function()
	-- 	M.refresh()
	-- end)
end

local function load_submodules()
	M.api = {
		-- sub = safe_require("shell.foo.sub"),
	}
end

local function build_runtime_state()
	local conf = cfg()

	return {
		cfg = conf,
		ui = ui(),
		shell = shell(),
		input = input(),
		system = system(),

		feature_cfg = conf.feature_name or {},

		-- sub = mod("sub"),
	}
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(args)
	runtime.ctx = args or runtime.ctx or {}

	if is_initialized() then
		return M
	end

	reset_state()
	load_submodules()

	runtime.state = build_runtime_state()

	-- Beispiel:
	-- init_module(mod("sub"), {
	-- 	cfg = cfg(),
	-- 	ui = ui(),
	-- 	module = M,
	-- })

	register_signals()

	M.public = build_public_api()
	M.input = build_input_branch()

	set_initialized(true)
	return M
end

function M.refresh()
	-- Optional: Runtime-Zustand neu materialisieren
	runtime.state = build_runtime_state()
	return M
end

function M.shutdown()
	-- Optional: Cleanup für Timer, Grabber, Handles, Signals
	-- Achtung: disconnect nur, wenn du Referenzen auf Callback-Funktionen hältst

	reset_state()
	set_signals_ready(false)
	set_initialized(false)

	M.public = {}
	M.input = {}

	return M
end

return M
