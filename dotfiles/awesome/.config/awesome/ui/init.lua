-- ~/.config/awesome/ui/init.lua
local function safe_require(path)
	local ok, mod = pcall(require, path)
	if ok then
		return mod
	end

	return nil
end

local M = {}

local runtime = {
	cfg = {},
}

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

local function maybe_export_theme_state(theme_state_mod, theme)
	if not (theme_state_mod and type(theme_state_mod.available) == "function") then
		return
	end

	if not theme_state_mod.available() then
		return
	end

	if type(theme_state_mod.export) == "function" then
		theme_state_mod.export(theme)
	end
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(args)
	args = args or {}
	runtime.cfg = args.cfg or args or {}

	local Helpers = safe_require("ui.lib.helpers")
	local ColorLib = safe_require("ui.lib.colors")
	local ThemeState = safe_require("ui.lib.theme_state")
	local ThemeApply = safe_require("ui.lib.theme_apply")
	local Themes = safe_require("ui.themes")
	local Wallpaper = safe_require("ui.wallpaper")

	local resolved = resolve_theme(Themes, runtime.cfg)
	local theme = materialize_theme(ColorLib, resolved)

	M.cfg = runtime.cfg

	M.lib = {
		helpers = Helpers or {},
		colors = ColorLib or {},
		theme_state = ThemeState or {},
		theme_apply = ThemeApply or {},
	}

	M.theme = theme
	M.wallpaper = Wallpaper or {}

	M.colors = theme.colors or {}
	M.fonts = theme.fonts or {}
	M.icons = theme.icons or {}
	M.utils = theme.utils or {}

	if ThemeApply and type(ThemeApply.init) == "function" then
		ThemeApply.init({
			apply_on_init = true,
		})
	end

	maybe_export_theme_state(ThemeState, theme)

	if Wallpaper and type(Wallpaper.init) == "function" then
		Wallpaper.init({
			cfg = runtime.cfg,
			ui = M,
		})
	end

	return M
end

return M
