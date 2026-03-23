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

local runtime_cfg = {}

-- =========================================================================
-- Helpers
-- =========================================================================

local function merge_shallow(a, b)
	local out = {}

	for k, v in pairs(a or {}) do
		out[k] = v
	end

	for k, v in pairs(b or {}) do
		out[k] = v
	end

	return out
end

local function resolve_theme(themes_mod, cfg)
	if not themes_mod or type(themes_mod.resolve) ~= "function" then
		return {}
	end

	return themes_mod.resolve(cfg or {}) or {}
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(args)
	args = args or {}
	runtime_cfg = args.cfg or args or {}

	local Helpers = safe_require("ui.helpers")
	local Colors = safe_require("ui.colors")
	local Themes = safe_require("ui.themes")
	local Wallpaper = safe_require("ui.wallpaper")

	local resolved_theme = resolve_theme(Themes, runtime_cfg)

	if Colors and type(Colors.set_runtime_cfg) == "function" then
		Colors.set_runtime_cfg(runtime_cfg)
	end

	local colors_api = Colors or {}
	local helpers_api = Helpers or {}
	local themes_api = Themes or {}

	local ui_api = {
		helpers = helpers_api,
		colors = colors_api,
		themes = themes_api,
		theme = resolved_theme,
	}

	if Wallpaper and type(Wallpaper.init) == "function" then
		Wallpaper.init({
			cfg = runtime_cfg,
			ui = ui_api,
		})
	end

	ui_api.wallpaper = Wallpaper

	M.api = ui_api
	M.cfg = runtime_cfg

	return M
end

function M.get()
	return M.api or {}
end

return M
