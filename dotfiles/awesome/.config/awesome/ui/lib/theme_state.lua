-- ~/.config/awesome/ui/lib/theme_state.lua
local gfs = require("gears.filesystem")

local M = {}

-- =========================================================================
-- JSON
-- =========================================================================

local function escape_string(s)
	s = tostring(s or "")
	s = s:gsub("\\", "\\\\")
	s = s:gsub('"', '\\"')
	s = s:gsub("\n", "\\n")
	s = s:gsub("\r", "\\r")
	s = s:gsub("\t", "\\t")
	return '"' .. s .. '"'
end

local function is_array(t)
	local max = 0
	local count = 0

	for k, _ in pairs(t) do
		if type(k) ~= "number" or k < 1 or k % 1 ~= 0 then
			return false
		end

		if k > max then
			max = k
		end

		count = count + 1
	end

	return max == count
end

local function indent(level)
	return string.rep("  ", level or 0)
end

local function encode_value(v, level)
	level = level or 0

	local tv = type(v)

	if tv == "nil" then
		return "null"
	end

	if tv == "boolean" then
		return v and "true" or "false"
	end

	if tv == "number" then
		return tostring(v)
	end

	if tv == "string" then
		return escape_string(v)
	end

	if tv == "table" then
		if is_array(v) then
			if #v == 0 then
				return "[]"
			end

			local out = {}

			for i = 1, #v do
				out[#out + 1] = indent(level + 1) .. encode_value(v[i], level + 1)
			end

			return "[\n" .. table.concat(out, ",\n") .. "\n" .. indent(level) .. "]"
		end

		local keys = {}
		for k, _ in pairs(v) do
			keys[#keys + 1] = k
		end
		table.sort(keys)

		if #keys == 0 then
			return "{}"
		end

		local out = {}

		for _, k in ipairs(keys) do
			out[#out + 1] = indent(level + 1) .. escape_string(k) .. ": " .. encode_value(v[k], level + 1)
		end

		return "{\n" .. table.concat(out, ",\n") .. "\n" .. indent(level) .. "}"
	end

	return "null"
end

local function encode_json(value)
	return encode_value(value, 0)
end

-- =========================================================================
-- Helpers
-- =========================================================================

local function state_path()
	return gfs.get_cache_dir() .. "theme-state.json"
end

local function ensure_cache_dir()
	pcall(function()
		gfs.make_directories(gfs.get_cache_dir())
	end)
end

local function sanitize_theme(theme)
	theme = theme or {}

	return {
		theme = {
			name = theme.name or "unknown",
			palette = theme.palette or {},
			roles = theme.roles or {},
			fonts = theme.fonts or {},
		},
	}
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.available()
	return true
end

function M.path()
	return state_path()
end

function M.export(theme)
	ensure_cache_dir()

	local payload = sanitize_theme(theme)
	local encoded = encode_json(payload)

	local f = io.open(state_path(), "w")
	if not f then
		return false, "open failed"
	end

	f:write(encoded)
	f:write("\n")
	f:close()

	if awesome and awesome.emit_signal then
		awesome.emit_signal("theme::state_updated", state_path())
	end

	return true
end

return M
