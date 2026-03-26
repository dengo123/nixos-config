-- ~/.config/awesome/ui/lib/theme_state.lua
local gfs = require("gears.filesystem")

local M = {}

local cjson_ok, cjson = pcall(require, "cjson")

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
			palette = theme.palette or {},
			roles = theme.roles or {},
			fonts = theme.fonts or {},
		},
	}
end

function M.available()
	return cjson_ok == true and cjson ~= nil
end

function M.path()
	return state_path()
end

function M.export(theme)
	if not M.available() then
		return false, "cjson unavailable"
	end

	ensure_cache_dir()

	local payload = sanitize_theme(theme)
	local ok, encoded = pcall(cjson.encode, payload)

	if not ok or type(encoded) ~= "string" or encoded == "" then
		return false, "json encode failed"
	end

	local f = io.open(state_path(), "w")
	if not f then
		return false, "open failed"
	end

	f:write(encoded)
	f:close()

	return true
end

return M
