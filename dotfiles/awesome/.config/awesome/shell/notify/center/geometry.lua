-- ~/.config/awesome/shell/notify/center/geometry.lua
local beautiful = require("beautiful")

local M = {}

-- =========================================================================
-- Helpers
-- =========================================================================

local function require_number(value, name)
	local n = tonumber(value)
	assert(n ~= nil, "notify.center.geometry: " .. name .. " fehlt/ungueltig")
	return n
end

local function require_table(value, name)
	assert(type(value) == "table", "notify.center.geometry: " .. name .. " fehlt/ungueltig")
	return value
end

local function center_theme()
	local notify = require_table(beautiful.notify, "beautiful.notify")
	return require_table(notify.center, "beautiful.notify.center")
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
		width_factor = require_number(t.width_factor, "beautiful.notify.center.width_factor"),
		min_width = require_number(t.min_width, "beautiful.notify.center.min_width"),
		max_width = require_number(t.max_width, "beautiful.notify.center.max_width"),

		min_height = require_number(t.min_height, "beautiful.notify.center.min_height"),
		max_height = require_number(t.max_height, "beautiful.notify.center.max_height"),

		offset_x = require_number(t.offset_x, "beautiful.notify.center.offset_x"),
		offset_y = require_number(t.offset_y, "beautiful.notify.center.offset_y"),

		margin_top = require_number(t.margin_top, "beautiful.notify.center.margin_top"),
		margin_right = require_number(t.margin_right, "beautiful.notify.center.margin_right"),
		margin_bottom = require_number(t.margin_bottom, "beautiful.notify.center.margin_bottom"),
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
