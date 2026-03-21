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

local function colors_api()
	return api().colors
end

local function helpers_api()
	return api().helpers
end

local function wallpaper_api()
	return api().wallpaper
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(cfg)
	cfg = cfg or {}

	M.api = {
		colors = safe_require("ui.colors"),
		helpers = safe_require("ui.helpers"),
		wallpaper = safe_require("ui.wallpaper"),
	}

	local Colors = colors_api()
	local Helpers = helpers_api()
	local Wallpaper = wallpaper_api()

	cfg.colors = cfg.colors or (Colors and Colors.get and Colors.get()) or {}
	cfg.helpers = cfg.helpers or Helpers

	if Wallpaper and type(Wallpaper.init) == "function" then
		Wallpaper.init({
			cfg = cfg,
			ui = M,
		})
	end

	return M
end

return M
