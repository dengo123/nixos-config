-- ~/.config/awesome/shell/notify/center/geometry.lua
local beautiful = require("beautiful")

local M = {}

-- =========================================================================
-- Helpers
-- =========================================================================

local function center_theme()
	local notify = beautiful.notify or {}
	return notify.center or {}
end

local function clamp(value, min_v, max_v)
	if value < min_v then
		return min_v
	end

	if value > max_v then
		return max_v
	end

	return value
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.build_theme()
	local t = center_theme()

	return {
		width_factor = tonumber(t.width_factor),
		min_width = tonumber(t.min_width),
		max_width = tonumber(t.max_width),

		min_height = tonumber(t.min_height),
		max_height = tonumber(t.max_height),

		offset_x = tonumber(t.offset_x),
		offset_y = tonumber(t.offset_y),

		margin_top = tonumber(t.margin_top),
		margin_right = tonumber(t.margin_right),
		margin_bottom = tonumber(t.margin_bottom),
	}
end

function M.resolve_width(theme, s)
	local wa = s.workarea
	return clamp(math.floor(wa.width * theme.width_factor), theme.min_width, theme.max_width)
end

function M.resolve_max_height(theme, s)
	local wa = s.workarea
	local h = wa.height - theme.margin_top - theme.margin_bottom
	return clamp(h, theme.min_height, theme.max_height)
end

function M.resolve_position(theme, s, bar_position, width, height)
	local wa = s.workarea

	local x = wa.x + wa.width - width - theme.margin_right + theme.offset_x
	local y

	if tostring(bar_position or "bottom") == "top" then
		y = wa.y + theme.margin_top + theme.offset_y
	else
		y = wa.y + wa.height - height - theme.margin_bottom + theme.offset_y
	end

	return {
		x = x,
		y = y,
		width = width,
		height = height,
	}
end

return M
