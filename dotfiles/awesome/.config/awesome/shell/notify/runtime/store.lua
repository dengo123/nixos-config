-- ~/.config/awesome/shell/notify/runtime/store.lua
local gears = require("gears")

local M = {}

local runtime = {
	path = nil,
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function ensure_parent_dir(path)
	if type(path) ~= "string" or path == "" then
		return
	end

	local dir = path:match("^(.*)/[^/]+$")
	if dir and dir ~= "" then
		gears.filesystem.make_directories(dir)
	end
end

local function lua_quote(value)
	return string.format("%q", tostring(value))
end

local function encode_lua(value, indent)
	indent = indent or 0

	local t = type(value)

	if t == "nil" then
		return "nil"
	end

	if t == "boolean" or t == "number" then
		return tostring(value)
	end

	if t == "string" then
		return lua_quote(value)
	end

	if t ~= "table" then
		return "nil"
	end

	local next_indent = indent + 1
	local pad = string.rep("\t", indent)
	local next_pad = string.rep("\t", next_indent)
	local parts = {}

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
			table.insert(parts, next_pad .. encode_lua(value[i], next_indent) .. ",")
		end

		return "{\n" .. table.concat(parts, "\n") .. "\n" .. pad .. "}"
	end

	local keys = {}

	for k, _ in pairs(value) do
		table.insert(keys, k)
	end

	table.sort(keys, function(a, b)
		return tostring(a) < tostring(b)
	end)

	for _, k in ipairs(keys) do
		local key_repr

		if type(k) == "string" and k:match("^[%a_][%w_]*$") then
			key_repr = k
		else
			key_repr = "[" .. encode_lua(k, next_indent) .. "]"
		end

		table.insert(parts, next_pad .. key_repr .. " = " .. encode_lua(value[k], next_indent) .. ",")
	end

	return "{\n" .. table.concat(parts, "\n") .. "\n" .. pad .. "}"
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(args)
	args = args or {}
	runtime.path = args.path or (gears.filesystem.get_cache_dir() .. "notify-history-state.lua")
	return M
end

function M.write(data)
	if type(runtime.path) ~= "string" or runtime.path == "" then
		return false
	end

	ensure_parent_dir(runtime.path)

	local file, err = io.open(runtime.path, "w")
	if not file then
		return false, err
	end

	file:write("return " .. encode_lua(data) .. "\n")
	file:close()

	return true
end

function M.read()
	if type(runtime.path) ~= "string" or runtime.path == "" then
		return nil
	end

	local chunk = loadfile(runtime.path)
	if not chunk then
		return nil
	end

	local ok, data = pcall(chunk)
	if not ok or type(data) ~= "table" then
		return nil
	end

	return data
end

return M
