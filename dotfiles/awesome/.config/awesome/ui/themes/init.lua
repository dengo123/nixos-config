-- ~/.config/awesome/ui/themes/init.lua
local M = {}

-- =========================================================================
-- Helpers
-- =========================================================================

local function safe_require(path)
	local ok, mod = pcall(require, path)
	if ok then
		return mod
	end

	return nil
end

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

local function merge_theme(base, override)
	base = base or {}
	override = override or {}

	return {
		name = override.name or base.name or "luna",
		colors = merge_shallow(base.colors or {}, override.colors or {}),
		fonts = merge_shallow(base.fonts or {}, override.fonts or {}),
		icons = merge_shallow(base.icons or {}, override.icons or {}),
		wallpaper = merge_shallow(base.wallpaper or {}, override.wallpaper or {}),
	}
end

local function theme_name_from_cfg(cfg)
	local ui_cfg = (cfg or {}).ui or {}
	local name = ui_cfg.theme

	if type(name) ~= "string" or name == "" then
		return "luna"
	end

	return name
end

local function load_theme(name)
	local mod = safe_require("ui.themes." .. tostring(name))
	if mod and type(mod.get) == "function" then
		return mod.get() or {}
	end

	return nil
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.resolve(cfg)
	local name = theme_name_from_cfg(cfg)

	local base = load_theme("luna")
		or {
			name = "luna",
			colors = {},
			fonts = {},
			icons = {},
			wallpaper = {},
		}

	if name == "luna" then
		return base
	end

	local override = load_theme(name)
	if not override then
		return base
	end

	return merge_theme(base, override)
end

return M
