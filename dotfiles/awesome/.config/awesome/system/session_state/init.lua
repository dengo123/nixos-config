-- ~/.config/awesome/system/session_state/init.lua
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
	restore_scheduled = false,
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function ctx()
	return runtime.ctx or {}
end

local function ensure_ctx_roots()
	local c = ctx()

	c.system = c.system or {}
	c.api = c.api or {}
	c.external = c.external or {}
	c.cfg = c.cfg or {}
	c.cfg.api = c.cfg.api or {}
end

local function api()
	return M.api or {}
end

local function mod(name)
	return api()[name]
end

local function init_module(name, args)
	local sub = mod(name)
	if sub and type(sub.init) == "function" then
		sub.init(args)
	end
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(args)
	runtime.ctx = args or {}
	ensure_ctx_roots()

	M.api = {
		store = safe_require("system.session_state.store"),
		snapshot = safe_require("system.session_state.snapshot"),
		restore = safe_require("system.session_state.restore"),
		signals = safe_require("system.session_state.signals"),
	}

	local c = ctx()

	c.system.session_state = M
	c.api.session_state = M
	c.external.session_state = M
	c.cfg.api.session_state = M

	local shared = {
		ctx = c,
		api = api(),
		cfg = c.cfg or {},
		ui = c.ui or {},
		system = c.system or {},
	}

	init_module("store", shared)
	init_module("snapshot", shared)
	init_module("restore", shared)
	init_module("signals", shared)

	return M
end

function M.snapshot()
	local Store = mod("store")
	local Snapshot = mod("snapshot")

	if not (Store and type(Store.write) == "function") then
		return false
	end

	if not (Snapshot and type(Snapshot.current_state) == "function") then
		return false
	end

	return Store.write(Snapshot.current_state())
end

function M.restore(opts)
	local Store = mod("store")
	local Restore = mod("restore")

	if not (Store and type(Store.read) == "function") then
		return false
	end

	if not (Restore and type(Restore.run) == "function") then
		return false
	end

	local data = Store.read()
	if not data then
		return false
	end

	return Restore.run(data, opts)
end

function M.restore_on_start()
	local Restore = mod("restore")

	if runtime.restore_scheduled then
		return
	end

	runtime.restore_scheduled = true

	if Restore and type(Restore.restore_on_start) == "function" then
		Restore.restore_on_start(function()
			runtime.restore_scheduled = false
			M.restore({
				restore_screen = false,
				restore_tag = false,
				restore_floating = false,
				restore_state = true,
			})
		end)
	else
		runtime.restore_scheduled = false
	end
end

function M.attach_signals()
	local Signals = mod("signals")

	if Signals and type(Signals.attach) == "function" then
		Signals.attach({
			snapshot = function()
				return M.snapshot()
			end,
			restore = function(opts)
				return M.restore(opts)
			end,
		})
	end
end

return M
