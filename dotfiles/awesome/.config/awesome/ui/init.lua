-- ~/.config/awesome/ui/init.lua
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

-- =========================================================================
-- Helpers
-- =========================================================================

local function api()
	return M.api or {}
end

local function module_api()
	return api().modules or {}
end

local function mod(name)
	return module_api()[name]
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(cfg)
	cfg = cfg or {}

	M.api = {
		modules = {
			colors = safe_require("ui.colors"),
			helpers = safe_require("ui.helpers"),
			wallpaper = safe_require("ui.wallpaper"),
		},
	}

	local Colors = mod("colors")
	local Helpers = mod("helpers")
	local Wallpaper = mod("wallpaper")

	cfg.colors = cfg.colors or (Colors and Colors.get and Colors.get()) or {}
	cfg.helpers = cfg.helpers or Helpers

	M.colors = cfg.colors
	M.helpers = cfg.helpers
	M.wallpaper = Wallpaper

	if Wallpaper and type(Wallpaper.init) == "function" then
		Wallpaper.init(cfg)
	end

	return M
end

return M
