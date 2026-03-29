-- ~/.config/awesome/system/session_state/store.lua
local gfs = require("gears.filesystem")

local M = {}

local runtime = {
	cfg = {},
	state_path = nil,
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function cfg()
	return runtime.cfg or {}
end

local function resolve_state_path(conf)
	local session_cfg = ((conf or {}).system or {}).session_state or {}
	local custom_path = session_cfg.path

	if type(custom_path) == "string" and custom_path ~= "" then
		return custom_path
	end

	return gfs.get_cache_dir() .. "session-state.lua"
end

local function ensure_cache_dir()
	local dir = gfs.get_dir(runtime.state_path or "")
	if dir and dir ~= "" then
		pcall(function()
			gfs.make_directories(dir)
		end)
	end
end

local function escape_string(s)
	s = tostring(s or "")
	s = s:gsub("\\", "\\\\")
	s = s:gsub("\n", "\\n")
	s = s:gsub("\r", "\\r")
	s = s:gsub("\t", "\\t")
	s = s:gsub('"', '\\"')
	return '"' .. s .. '"'
end

local function is_identifier(s)
	return type(s) == "string" and s:match("^[%a_][%w_]*$") ~= nil
end

local function serialize(value, seen)
	seen = seen or {}

	local t = type(value)

	if t == "nil" then
		return "nil"
	end

	if t == "boolean" then
		return value and "true" or "false"
	end

	if t == "number" then
		if value ~= value or value == math.huge or value == -math.huge then
			return "0"
		end
		return tostring(value)
	end

	if t == "string" then
		return escape_string(value)
	end

	if t ~= "table" then
		return "nil"
	end

	if seen[value] then
		return "nil"
	end
	seen[value] = true

	local out = {}
	local is_array = true
	local max_index = 0

	for k, _ in pairs(value) do
		if type(k) ~= "number" or k < 1 or k % 1 ~= 0 then
			is_array = false
			break
		end

		if k > max_index then
			max_index = k
		end
	end

	if is_array then
		for i = 1, max_index do
			table.insert(out, serialize(value[i], seen))
		end

		seen[value] = nil
		return "{ " .. table.concat(out, ", ") .. " }"
	end

	for k, v in pairs(value) do
		local key
		if is_identifier(k) then
			key = k
		else
			key = "[" .. serialize(k, seen) .. "]"
		end

		table.insert(out, key .. " = " .. serialize(v, seen))
	end

	table.sort(out)

	seen[value] = nil
	return "{ " .. table.concat(out, ", ") .. " }"
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(args)
	args = args or {}

	runtime.cfg = args.cfg or runtime.cfg or {}
	runtime.state_path = resolve_state_path(cfg())

	return M
end

function M.path()
	return runtime.state_path or resolve_state_path(cfg())
end

function M.read()
	local path = M.path()
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

function M.write(data)
	if type(data) ~= "table" then
		return false
	end

	local path = M.path()
	ensure_cache_dir()

	local payload = "return " .. serialize(data) .. "\n"

	local f = io.open(path, "w")
	if not f then
		return false
	end

	f:write(payload)
	f:close()

	return true
end

return M
