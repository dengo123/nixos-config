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

local function resolve_theme(themes_mod, cfg)
	assert(themes_mod and type(themes_mod.resolve) == "function", "ui.init: ui.themes.resolve fehlt")
	return themes_mod.resolve(cfg or {}) or {}
end

local function materialize_theme(colors_lib, resolved_theme)
	assert(colors_lib and type(colors_lib.build) == "function", "ui.init: ui.lib.colors.build fehlt")

	local built = colors_lib.build(resolved_theme or {})
	local colors = built.colors or {}
	local utils = built.utils or {}

	return {
		name = resolved_theme.name or "luna",
		palette = resolved_theme.palette or {},
		roles = resolved_theme.roles or {},
		colors = colors,
		fonts = resolved_theme.fonts or {},
		icons = resolved_theme.icons or {},
		wallpaper = resolved_theme.wallpaper or {},
		utils = utils,
	}
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(args)
	args = args or {}
	runtime_cfg = args.cfg or args or {}

	local Helpers = safe_require("ui.lib.helpers")
	local ColorLib = safe_require("ui.lib.colors")
	local Themes = safe_require("ui.themes")
	local Wallpaper = safe_require("ui.wallpaper")

	local resolved = resolve_theme(Themes, runtime_cfg)
	local theme = materialize_theme(ColorLib, resolved)

	local ui_api = {
		lib = {
			helpers = Helpers or {},
			colors = ColorLib or {},
		},
		theme = theme,
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
