-- ~/.config/awesome/shell/notify/runtime/init.lua
local function safe_require(path)
	local ok, mod = pcall(require, path)
	if ok then
		return mod
	end

	return nil
end

local M = {
	history = nil,
	pipeline = nil,
	store = nil,
	restore_mod = nil,
	signals = nil,
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
	c.shell = c.shell or {}
	c.cfg = c.cfg or {}
	c.ui = c.ui or {}
end

local function init_module(mod, args)
	if mod and type(mod.init) == "function" then
		mod.init(args)
	end
end

local function notify_cfg()
	return (ctx().cfg or {}).notify or {}
end

local function history_cfg()
	return notify_cfg().history or {}
end

local function history_max_entries()
	local hcfg = history_cfg()
	return hcfg.max_entries
end

local function history_store_path()
	local hcfg = history_cfg()
	return hcfg.path
end

local function restore_on_start_enabled()
	return history_cfg().restore_on_start ~= false
end

local function register_pipeline()
	local Pipeline = M.pipeline
	local c = ctx()

	if Pipeline and type(Pipeline.register) == "function" then
		Pipeline.register({
			cfg = c.cfg or {},
			history = M.history,
		})
	end
end

local function shared_args()
	local c = ctx()

	return {
		ctx = c,
		cfg = c.cfg or {},
		ui = c.ui or {},
		shell = c.shell or {},
		notify = c.notify,
		notify_theme = c.notify_theme or {},
		runtime = M,
		history = M.history,
		store = M.store,
		restore = M.restore_mod,
	}
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(args)
	runtime.ctx = args or {}
	ensure_ctx_roots()

	M.history = safe_require("shell.notify.runtime.history")
	M.pipeline = safe_require("shell.notify.runtime.pipeline")
	M.store = safe_require("shell.notify.runtime.store")
	M.restore_mod = safe_require("shell.notify.runtime.restore")
	M.signals = safe_require("shell.notify.runtime.signals")

	local c = ctx()
	c.shell.notify = c.shell.notify or {}
	c.shell.notify.runtime = M

	init_module(M.history, {
		max_entries = history_max_entries(),
	})

	init_module(M.store, {
		path = history_store_path(),
	})

	init_module(M.restore_mod, {
		history = M.history,
	})

	init_module(M.signals, shared_args())

	register_pipeline()

	return M
end

function M.snapshot()
	local Store = M.store
	local History = M.history

	if not (Store and type(Store.write) == "function") then
		return false
	end

	if not (History and type(History.export_state) == "function") then
		return false
	end

	return Store.write(History.export_state())
end

function M.restore()
	local Store = M.store
	local Restore = M.restore_mod

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
	local Restore = M.restore_mod

	if not restore_on_start_enabled() then
		return
	end

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
	local Signals = M.signals

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
