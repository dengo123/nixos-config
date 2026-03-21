-- ~/.config/awesome/ui/wallpaper/scope.lua
local M = {}

-- =========================================================================
-- Helpers
-- =========================================================================

local function right_edge(g)
	return (g.x or 0) + (g.width or 0)
end

local function bottom_edge(g)
	return (g.y or 0) + (g.height or 0)
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.virtual_geometry()
	local min_x, min_y, max_x, max_y

	for s in screen do
		local g = s.geometry or {}

		min_x = (min_x == nil) and (g.x or 0) or math.min(min_x, g.x or 0)
		min_y = (min_y == nil) and (g.y or 0) or math.min(min_y, g.y or 0)
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

function M.screen_offset(s, vg)
	local g = (s and s.geometry) or {}
	vg = vg or M.virtual_geometry()

	return {
		x = (g.x or 0) - (vg.x or 0),
		y = (g.y or 0) - (vg.y or 0),
	}
end

function M.screen_geometry_in_virtual_space(s, vg)
	local g = (s and s.geometry) or {}
	local off = M.screen_offset(s, vg)

	return {
		x = off.x,
		y = off.y,
		width = g.width or 0,
		height = g.height or 0,
	}
end

function M.is_primary_screen(s)
	return s == (screen.primary or nil)
end

function M.is_portrait_screen(s)
	return s and s.geometry and s.geometry.height > s.geometry.width
end

function M.is_landscape_screen(s)
	return s and s.geometry and s.geometry.width >= s.geometry.height
end

function M.is_left_of_primary(s)
	local primary = screen.primary
	if not (s and primary and primary.geometry and s.geometry) then
		return false
	end

	return (s.geometry.x or 0) < (primary.geometry.x or 0)
end

function M.is_right_of_primary(s)
	local primary = screen.primary
	if not (s and primary and primary.geometry and s.geometry) then
		return false
	end

	return (s.geometry.x or 0) > (primary.geometry.x or 0)
end

function M.is_above_primary(s)
	local primary = screen.primary
	if not (s and primary and primary.geometry and s.geometry) then
		return false
	end

	return (s.geometry.y or 0) < (primary.geometry.y or 0)
end

function M.is_below_primary(s)
	local primary = screen.primary
	if not (s and primary and primary.geometry and s.geometry) then
		return false
	end

	return (s.geometry.y or 0) > (primary.geometry.y or 0)
end

return M
