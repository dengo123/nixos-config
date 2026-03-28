-- ~/.config/awesome/ui/wallpaper/controller.lua
local beautiful = require("beautiful")
local gfs = require("gears.filesystem")
local Themes = require("ui.themes")

local M = {}

local runtime = {
	cfg = {},
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function wallpaper_cfg()
	local ui_cfg = runtime.cfg.ui or {}
	return ui_cfg.wallpaper or {}
end

local function deepcopy(value)
	if type(value) ~= "table" then
		return value
	end

	local out = {}

	for k, v in pairs(value) do
		out[deepcopy(k)] = deepcopy(v)
	end

	return out
end

local function merge_into(dst, src)
	for k, v in pairs(src or {}) do
		if type(v) == "table" and type(dst[k]) == "table" then
			merge_into(dst[k], v)
		else
			dst[k] = deepcopy(v)
		end
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

local function theme_wallpaper_source()
	local theme = Themes.resolve(runtime.cfg)
	local wallpaper = (theme and theme.wallpaper) or {}
	return wallpaper.source
end

local function resolve_source(source)
	local expanded = expand_source_path(source)
	if expanded and expanded ~= "" then
		return expanded
	end

	local theme_source = expand_source_path(theme_wallpaper_source())
	if theme_source and theme_source ~= "" then
		return theme_source
	end

	if type(beautiful.wallpaper) == "string" and beautiful.wallpaper ~= "" then
		return beautiful.wallpaper
	end

	return nil
end

local function normalize_rotation(rotation)
	rotation = rotation or {}

	return {
		enabled = (rotation.enabled == true),
		interval = tonumber(rotation.interval) or 600,
		random = (rotation.random == true),
	}
end

local function normalize_display(display)
	display = display or {}

	local mode = tostring(display.mode or "maximized"):lower()
	local fill = tostring(display.fill or "solid"):lower()

	if mode ~= "maximized" and mode ~= "fit_fill" and mode ~= "center" and mode ~= "stretch" then
		mode = "maximized"
	end

	if fill ~= "solid" and fill ~= "gradient" then
		fill = "solid"
	end

	return {
		mode = mode,
		fill = fill,
		span_across_screens = (display.span_across_screens == true),
	}
end

local function screen_is_portrait(s)
	return s and s.geometry and (s.geometry.height or 0) > (s.geometry.width or 0)
end

local function screen_is_landscape(s)
	return s and s.geometry and (s.geometry.width or 0) >= (s.geometry.height or 0)
end

local function normalize_match(match)
	if match == nil then
		return {}
	end

	if type(match) == "string" then
		match = { orientation = match }
	end

	match = match or {}

	local out = {}

	if match.index ~= nil then
		out.index = tonumber(match.index)
	end

	if match.orientation ~= nil then
		local orientation = tostring(match.orientation):lower()
		if orientation == "portrait" or orientation == "landscape" then
			out.orientation = orientation
		end
	end

	if match.primary ~= nil then
		out.primary = (match.primary == true)
	end

	return out
end

local function match_screen(s, match)
	match = normalize_match(match)

	if match.index ~= nil and (not s or s.index ~= match.index) then
		return false
	end

	if match.orientation == "portrait" and not screen_is_portrait(s) then
		return false
	end

	if match.orientation == "landscape" and not screen_is_landscape(s) then
		return false
	end

	if match.primary == true and s ~= screen.primary then
		return false
	end

	if match.primary == false and s == screen.primary then
		return false
	end

	return true
end

local function base_spec(cfg)
	return {
		source = cfg.source,
		display = normalize_display(cfg.display),
		rotation = normalize_rotation(cfg.rotation),
	}
end

local function apply_rule_to_spec(spec, rule)
	if type(rule.source) == "string" and rule.source ~= "" then
		spec.source = rule.source
	end

	if type(rule.display) == "table" then
		spec.display = normalize_display(merge_into(deepcopy(spec.display or {}), rule.display))
	end

	if type(rule.rotation) == "table" then
		spec.rotation = normalize_rotation(merge_into(deepcopy(spec.rotation or {}), rule.rotation))
	end

	return spec
end

local function apply_matching_rules(spec, s, cfg)
	for _, rule in ipairs(cfg.rules or {}) do
		if type(rule) == "table" and match_screen(s, rule.match) then
			apply_rule_to_spec(spec, rule)
		end
	end

	return spec
end

local function finalize_spec(spec)
	spec.source = resolve_source(spec.source)
	spec.display = normalize_display(spec.display)
	spec.rotation = normalize_rotation(spec.rotation)
	spec.fit_applies = (spec.display.mode == "fit_fill")
	spec.span_across_screens = (spec.display.span_across_screens == true)
	return spec
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.set_runtime_cfg(cfg)
	runtime.cfg = cfg or {}
end

function M.get()
	return wallpaper_cfg()
end

function M.screen_spec(s)
	local cfg = wallpaper_cfg()
	local spec = base_spec(cfg)

	spec = apply_matching_rules(spec, s, cfg)
	spec = finalize_spec(spec)

	return spec
end

function M.screen_spans_desktop(s)
	local spec = M.screen_spec(s)
	return spec and spec.span_across_screens == true or false
end

function M.screens_for_desktop_span()
	local out = {}

	for s in screen do
		local spec = M.screen_spec(s)

		if spec and spec.span_across_screens == true then
			table.insert(out, {
				screen = s,
				spec = spec,
			})
		end
	end

	return out
end

function M.match_screen(s, match)
	return match_screen(s, match)
end

return M
