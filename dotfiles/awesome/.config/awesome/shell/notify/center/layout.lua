-- ~/.config/awesome/shell/notify/center/layout.lua
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

	if max_v ~= nil and value > max_v then
		return max_v
	end

	return value
end

local function bar_cfg(cfg)
	return (cfg and cfg.bar) or {}
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

		list_pad_left = tonumber(t.list_pad_left) or 0,
		list_pad_right = tonumber(t.list_pad_right) or 0,
	}
end

function M.resolve_width(theme, s)
	local wa = s.workarea
	local width_factor = tonumber(theme.width_factor) or 0.30
	local min_width = tonumber(theme.min_width) or 320
	local max_width = tonumber(theme.max_width)

	local width = math.floor(wa.width * width_factor)
	return clamp(width, min_width, max_width)
end

function M.resolve_max_height(theme, s)
	local wa = s.workarea
	local usable_h = wa.height - (tonumber(theme.margin_top) or 0) - (tonumber(theme.margin_bottom) or 0)
	local min_height = tonumber(theme.min_height) or 1
	local max_height = tonumber(theme.max_height)

	return clamp(usable_h, min_height, max_height)
end

function M.resolve_position(theme, s, bar_position, width, height)
	local wa = s.workarea

	local x = wa.x + wa.width - width - (tonumber(theme.margin_right) or 0) + (tonumber(theme.offset_x) or 0)
	local y

	if tostring(bar_position or "bottom") == "top" then
		y = wa.y + (tonumber(theme.margin_top) or 0) + (tonumber(theme.offset_y) or 0)
	else
		y = wa.y + wa.height - height - (tonumber(theme.margin_bottom) or 0) + (tonumber(theme.offset_y) or 0)
	end

	return {
		x = x,
		y = y,
		width = width,
		height = height,
	}
end

function M.resolve_geometry(args)
	args = args or {}

	local popup = args.popup
	local cfg = args.cfg or {}
	local height_override = args.height_override

	if not (popup and popup.screen) then
		return nil
	end

	local theme = M.build_theme()
	local width = M.resolve_width(theme, popup.screen)
	local max_height = M.resolve_max_height(theme, popup.screen)
	local bar_position = tostring(bar_cfg(cfg).position or "bottom")

	local height = tonumber(height_override) or max_height
	if max_height > 0 and height > max_height then
		height = max_height
	end

	if height <= 0 then
		return {
			x = 0,
			y = 0,
			width = width,
			height = 0,
		}
	end

	return M.resolve_position(theme, popup.screen, bar_position, width, height)
end

function M.resolve_list_width(popup_width)
	local theme = M.build_theme()

	return math.max(1, (tonumber(popup_width) or 1) - (theme.list_pad_left or 0) - (theme.list_pad_right or 0))
end

return M
