-- ~/.config/awesome/ui/wallpaper/scope.lua
local M = {}

local runtime_cfg = {}

-- =========================================================================
-- Helpers
-- =========================================================================

local function screen_geometry(s)
	local g = (s and s.geometry) or {}

	return {
		x = tonumber(g.x) or 0,
		y = tonumber(g.y) or 0,
		width = tonumber(g.width) or 0,
		height = tonumber(g.height) or 0,
	}
end

local function right_edge(g)
	return (g.x or 0) + (g.width or 0)
end

local function bottom_edge(g)
	return (g.y or 0) + (g.height or 0)
end

local function screen_is_portrait(s)
	local g = screen_geometry(s)
	return g.height > g.width
end

local function screen_is_landscape(s)
	local g = screen_geometry(s)
	return g.width >= g.height
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

local function wallpaper_cfg()
	local ui_cfg = runtime_cfg.ui or {}
	return ui_cfg.wallpaper or {}
end

local function display_span_enabled(display)
	return type(display) == "table" and display.span_across_screens == true
end

local function screen_final_span_state(s)
	local cfg = wallpaper_cfg()
	local span = display_span_enabled(cfg.display)

	for _, rule in ipairs(cfg.rules or {}) do
		if type(rule) == "table" and match_screen(s, rule.match) then
			if type(rule.display) == "table" and rule.display.span_across_screens ~= nil then
				span = (rule.display.span_across_screens == true)
			end
		end
	end

	return span
end

local function screens_in_same_span_group(spec)
	local out = {}

	local want_span = spec and spec.display and spec.display.span_across_screens == true

	if not want_span then
		return out
	end

	for s in screen do
		if screen_final_span_state(s) == true then
			table.insert(out, s)
		end
	end

	return out
end

local function virtual_geometry_for_screens(screens)
	local min_x, min_y, max_x, max_y

	for _, s in ipairs(screens or {}) do
		local g = screen_geometry(s)

		min_x = (min_x == nil) and g.x or math.min(min_x, g.x)
		min_y = (min_y == nil) and g.y or math.min(min_y, g.y)
		max_x = (max_x == nil) and right_edge(g) or math.max(max_x, right_edge(g))
		max_y = (max_y == nil) and bottom_edge(g) or math.max(max_y, bottom_edge(g))
	end

	if min_x == nil then
		return {
			x = 0,
			y = 0,
			width = 0,
			height = 0,
		}
	end

	return {
		x = min_x,
		y = min_y,
		width = math.max(0, max_x - min_x),
		height = math.max(0, max_y - min_y),
	}
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.set_runtime_cfg(cfg)
	runtime_cfg = cfg or {}
end

function M.virtual_geometry()
	local all = {}

	for s in screen do
		table.insert(all, s)
	end

	return virtual_geometry_for_screens(all)
end

function M.screen_offset(s, vg)
	local g = screen_geometry(s)
	vg = vg or M.virtual_geometry()

	return {
		x = g.x - (vg.x or 0),
		y = g.y - (vg.y or 0),
	}
end

function M.geometry_for_span_group(s, spec)
	local screens = screens_in_same_span_group(spec)

	if #screens == 0 then
		local vg = M.virtual_geometry()
		return vg, M.screen_offset(s, vg)
	end

	local vg = virtual_geometry_for_screens(screens)

	return vg, {
		x = screen_geometry(s).x - vg.x,
		y = screen_geometry(s).y - vg.y,
	}
end

function M.is_primary_screen(s)
	return s == screen.primary
end

function M.is_portrait_screen(s)
	return screen_is_portrait(s)
end

function M.is_landscape_screen(s)
	return screen_is_landscape(s)
end

return M
