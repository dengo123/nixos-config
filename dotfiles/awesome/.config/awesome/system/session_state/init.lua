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
	restore_scheduled = false,
}

-- =========================================================================
-- Helpers
-- =========================================================================

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
	args = args or {}

	M.api = {
		store = safe_require("system.session_state.store"),
		snapshot = safe_require("system.session_state.snapshot"),
		restore = safe_require("system.session_state.restore"),
		signals = safe_require("system.session_state.signals"),
	}

	local shared = {
		api = api(),
		cfg = args.cfg or {},
		ui = args.ui or {},
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

function M.restore()
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

	return Restore.run(data)
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
			M.restore()
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
			restore = function()
				return M.restore()
			end,
		})
	end
end

return M
