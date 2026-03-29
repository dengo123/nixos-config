-- ~/.config/awesome/<path>/runtime_module_skeleton.lua
local M = {}

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

local function state()
	return runtime.state or {}
end

local function set_state(next_state)
	runtime.state = next_state or {}
	return runtime.state
end

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
-- Helpers: Safety
-- =========================================================================

local function safe_call(fn, ...)
	if type(fn) ~= "function" then
		return false, nil
	end

	return pcall(fn, ...)
end

local function clamp(v, lo, hi)
	if v < lo then
		return lo
	end

	if v > hi then
		return hi
	end

	return v
end

-- =========================================================================
-- Internal state builders
-- =========================================================================

local function build_state()
	local c = ctx()

	return {
		cfg = c.cfg or {},
		ui = c.ui or {},
		shell = c.shell or {},
		module_cfg = (c.cfg or {}).example_runtime or {},

		-- eigener interner Zustand:
		enabled = true,
		active = false,
		cache = {},
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
	-- awesome.connect_signal("example::refresh", function()
	-- 	M.refresh()
	-- end)
end

local function unregister_signals()
	-- Nur notwendig, wenn du Callback-Referenzen speicherst
	set_signals_ready(false)
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(args)
	runtime.ctx = args or runtime.ctx or {}

	if is_initialized() then
		return M
	end

	set_state(build_state())
	register_signals()

	set_initialized(true)
	return M
end

function M.apply(args)
	runtime.ctx = args or runtime.ctx or {}
	set_state(build_state())
	return M
end

function M.refresh()
	set_state(build_state())
	return M
end

function M.attach_signals()
	register_signals()
	return M
end

function M.reset()
	local s = state()
	s.active = false
	s.cache = {}
	return M
end

function M.enable()
	state().enabled = true
	return M
end

function M.disable()
	state().enabled = false
	return M
end

function M.is_enabled()
	return state().enabled == true
end

function M.is_active()
	return state().active == true
end

function M.activate()
	local s = state()

	if s.enabled == false then
		return false
	end

	s.active = true
	return true
end

function M.deactivate()
	state().active = false
	return true
end

function M.get_state()
	return state()
end

function M.shutdown()
	unregister_signals()
	set_state({})
	set_initialized(false)
	return M
end

return M
