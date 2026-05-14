-- ~/.config/awesome/system/session_state/init.lua
local function safe_require(path)
	local ok, mod = pcall(require, path)
	if ok then
		return mod
	end

	return nil
end

local M = {
	store = nil,
	snapshot_mod = nil,
	restore_mod = nil,
	signals = nil,
	app_restore = nil,
	startup = nil,
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
	c.cfg = c.cfg or {}
end

local function init_module(mod, args)
	if mod and type(mod.init) == "function" then
		mod.init(args)
	end
end

local function session_state_cfg()
	return ((ctx().cfg or {}).system or {}).session_state or {}
end

local function restore_cfg()
	return session_state_cfg().restore or {}
end

local function restore_on_start_enabled()
	return restore_cfg().on_start ~= false
end

local function restore_opts_from_cfg()
	local rcfg = restore_cfg()

	return {
		restore_screen = (rcfg.screen == true),
		restore_tag = (rcfg.tag == true),
		restore_state = (rcfg.state ~= false),
		restore_layout = (rcfg.layout ~= false),
	}
end

local function shared_args()
	local c = ctx()

	return {
		ctx = c,
		cfg = c.cfg or {},
		ui = c.ui or {},
		shell = c.shell or {},
		input = c.input or {},
		system = c.system or {},
		modkey = c.modkey,
		session_state = M,
		store = M.store,
	}
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(args)
	runtime.ctx = args or {}
	ensure_ctx_roots()

	M.store = safe_require("system.session_state.store")
	M.snapshot_mod = safe_require("system.session_state.snapshot")
	M.restore_mod = safe_require("system.session_state.restore")
	M.signals = safe_require("system.session_state.signals")
	M.app_restore = safe_require("system.session_state.app_restore")
	M.startup = safe_require("system.session_state.startup")

	local c = ctx()
	c.system.session_state = M

	local shared = shared_args()

	init_module(M.store, shared)
	init_module(M.snapshot_mod, shared)
	init_module(M.restore_mod, shared)
	init_module(M.signals, shared)
	init_module(M.app_restore, shared)
	init_module(M.startup, shared)

	return M
end

function M.snapshot()
	local Store = M.store
	local Snapshot = M.snapshot_mod

	if not (Store and type(Store.write) == "function") then
		return false
	end

	if not (Snapshot and type(Snapshot.current_state) == "function") then
		return false
	end

	return Store.write(Snapshot.current_state())
end

function M.restore(opts)
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

	local merged_opts = restore_opts_from_cfg()
	for k, v in pairs(opts or {}) do
		merged_opts[k] = v
	end

	return Restore.run(data, merged_opts)
end

function M.restore_on_start()
	local Startup = M.startup
	local AppRestore = M.app_restore

	if not restore_on_start_enabled() then
		return
	end

	if runtime.restore_scheduled then
		return
	end

	runtime.restore_scheduled = true

	local finalize = function()
		runtime.restore_scheduled = false

		if Startup and type(Startup.run) == "function" then
			Startup.run({
				restore = function(opts)
					return M.restore(opts)
				end,
				app_restore = function()
					if AppRestore and type(AppRestore.run) == "function" then
						return AppRestore.run()
					end

					return false
				end,
			})
			return
		end

		M.restore({
			retry_passes = true,
		})

		if AppRestore and type(AppRestore.run) == "function" then
			AppRestore.run()
		end
	end

	if M.restore_mod and type(M.restore_mod.restore_on_start) == "function" then
		M.restore_mod.restore_on_start(finalize)
	else
		finalize()
	end
end

function M.attach_signals()
	local Signals = M.signals
	local AppRestore = M.app_restore
	local Startup = M.startup

	if Signals and type(Signals.attach) == "function" then
		Signals.attach({
			snapshot = function()
				return M.snapshot()
			end,
			restore = function(opts)
				return M.restore(opts)
			end,
			on_exit = function(is_restart)
				if Startup and type(Startup.note_exit) == "function" then
					Startup.note_exit(is_restart)
				end

				if AppRestore and type(AppRestore.note_exit) == "function" then
					AppRestore.note_exit(is_restart)
				end
			end,
		})
	end
end

return M
