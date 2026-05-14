-- ~/.config/awesome/system/session_state/startup.lua
local gears = require("gears")

local M = {}

local runtime = {
	cfg = {},
	restart_marker_path = nil,
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function cfg()
	return runtime.cfg or {}
end

local function session_state_cfg()
	return ((cfg() or {}).system or {}).session_state or {}
end

local function restore_cfg()
	return session_state_cfg().restore or {}
end

local function restore_on_start_enabled()
	return restore_cfg().on_start ~= false
end

local function restart_marker_path()
	if type(runtime.restart_marker_path) == "string" and runtime.restart_marker_path ~= "" then
		return runtime.restart_marker_path
	end

	return gears.filesystem.get_cache_dir() .. "session-state-restart-marker.lua"
end

local function write_restart_marker(data)
	local path = restart_marker_path()
	local file = io.open(path, "w")
	if not file then
		return false
	end

	file:write("return {\n")
	file:write("\tskip_restore_once = " .. tostring(data and data.skip_restore_once == true) .. ",\n")
	file:write("}\n")
	file:close()

	return true
end

local function read_restart_marker()
	local path = restart_marker_path()
	local chunk = loadfile(path)
	if not chunk then
		return nil
	end

	local ok, data = pcall(chunk)
	if not ok or type(data) ~= "table" then
		return nil
	end

	return data
end

local function clear_restart_marker()
	os.remove(restart_marker_path())
end

local function start_mode_for_this_start()
	local marker = read_restart_marker()
	if not marker then
		return "login"
	end

	local is_reload = (marker.skip_restore_once == true)
	clear_restart_marker()

	if is_reload then
		return "reload"
	end

	return "login"
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(args)
	args = args or {}
	runtime.cfg = args.cfg or runtime.cfg or {}
	runtime.restart_marker_path = args.restart_marker_path or runtime.restart_marker_path
	return M
end

function M.note_exit(is_restart)
	if is_restart == true then
		write_restart_marker({
			skip_restore_once = true,
		})
	else
		clear_restart_marker()
	end
end

function M.run(args)
	args = args or {}

	if not restore_on_start_enabled() then
		return false
	end

	local restore = args.restore
	local app_restore = args.app_restore

	if type(restore) ~= "function" then
		return false
	end

	local mode = start_mode_for_this_start()

	if mode == "reload" then
		restore({
			retry_passes = false,
		})
		return true
	end

	restore({
		retry_passes = true,
	})

	if type(app_restore) == "function" then
		app_restore()
	end

	return true
end

return M
