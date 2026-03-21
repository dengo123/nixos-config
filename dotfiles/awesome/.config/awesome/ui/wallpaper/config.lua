-- ~/.config/awesome/ui/wallpaper/config.lua
local beautiful = require("beautiful")
local gfs = require("gears.filesystem")

local M = {}

local runtime_cfg = {}

-- =========================================================================
-- Helpers
-- =========================================================================

local function wallpaper_cfg()
	local ui_cfg = runtime_cfg.ui or {}
	return ui_cfg.wallpaper or {}
end

local function fallback_source()
	return gfs.get_configuration_dir() .. "ui/assets/bliss2d.png"
end

local function merge_into(dst, src)
	for k, v in pairs(src or {}) do
		dst[k] = v
	end

	return dst
end

local function expand_source_path(source)
	source = tostring(source or "")

	if source == "" then
		return nil
	end

	if source:match("^~/") then
		local home = os.getenv("HOME") or ""
		return home .. source:sub(2)
	end

	if source:match("^/") then
		return source
	end

	return gfs.get_configuration_dir() .. source
end

local function resolve_source(source)
	local expanded = expand_source_path(source)
	if expanded and expanded ~= "" then
		return expanded
	end

	if type(beautiful.wallpaper) == "string" and beautiful.wallpaper ~= "" then
		return beautiful.wallpaper
	end

	return fallback_source()
end

local function normalize_rotation(rotation)
	rotation = rotation or {}

	return {
		enabled = (rotation.enabled == true),
		interval = tonumber(rotation.interval) or 600,
		random = (rotation.random == true),
	}
end

local function normalize_fit(fit)
	if fit == true then
		return {
			enabled = true,
			style = "solid",
		}
	end

	fit = fit or {}

	return {
		enabled = (fit.enabled == true),
		style = tostring(fit.style or "solid"):lower(),
	}
end

local function base_spec(cfg)
	return {
		source = cfg.source,
		fit = normalize_fit(cfg.fit),
		rotation = normalize_rotation(cfg.rotation),
	}
end

local function finalize_spec(spec)
	spec.source = resolve_source(spec.source)
	spec.fit = normalize_fit(spec.fit)
	spec.rotation = normalize_rotation(spec.rotation)
	return spec
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.set_runtime_cfg(cfg)
	runtime_cfg = cfg or {}
end

function M.get()
	return wallpaper_cfg()
end

function M.span_across_screens()
	local cfg = wallpaper_cfg()
	return cfg.span_across_screens == true
end

function M.global_spec()
	local cfg = wallpaper_cfg()
	return finalize_spec(base_spec(cfg))
end

function M.screen_spec(s)
	local cfg = wallpaper_cfg()
	local spec = base_spec(cfg)

	if cfg.span_across_screens ~= true and s then
		local screen_cfg = cfg.screen or {}
		local index_cfg = screen_cfg[s.index]

		if index_cfg then
			merge_into(spec, index_cfg)
		end
	end

	return finalize_spec(spec)
end

return M
