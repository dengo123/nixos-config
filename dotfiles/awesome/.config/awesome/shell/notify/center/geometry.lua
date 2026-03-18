-- ~/.config/awesome/shell/notify/center/geometry.lua
local awful = require("awful")

local M = {}

-- =========================================================================
-- Helpers
-- =========================================================================

local function clamp(value, min_v, max_v)
	if value < min_v then
		return min_v
	end

	if value > max_v then
		return max_v
	end

	return value
end

local function key_for_screen(s)
	return tostring(s.index or 1)
end

local function target_screens(cfg)
	local bar_cfg = cfg.bar or {}
	local mode = tostring(bar_cfg.show_notify or "primary")

	if mode == "all" then
		local out = {}
		for s in screen do
			table.insert(out, s)
		end
		return out
	end

	return { screen.primary or awful.screen.focused() }
end

local function resolve_entries_count(entries, visible_entries)
	return math.min(#entries, visible_entries)
end

local function resolve_body_h(theme, entries, visible_entries)
	local count = resolve_entries_count(entries, visible_entries)

	if count <= 0 then
		return theme.empty_h
	end

	return (count * theme.entry_h) + ((count - 1) * theme.spacing)
end

local function resolve_width(theme, s)
	local wa = s.workarea

	return clamp(math.floor(wa.width * theme.width_factor), theme.min_width, theme.max_width)
end

local function resolve_height_limit(theme, s)
	local wa = s.workarea

	return clamp(math.floor(wa.height * theme.height_factor), theme.min_height, theme.max_height)
end

local function resolve_panel_h(theme, s, entries, visible_entries)
	local content_h = theme.header_h + resolve_body_h(theme, entries, visible_entries)
	return math.min(content_h, resolve_height_limit(theme, s))
end

local function resolve_geometry(cfg, theme, s, entries, visible_entries)
	local bar_cfg = cfg.bar or {}
	local bar_position = tostring(bar_cfg.position or "bottom")

	local wa = s.workarea
	local width = resolve_width(theme, s)
	local height = resolve_panel_h(theme, s, entries, visible_entries)

	local x = wa.x + wa.width - width - theme.margin_right + theme.offset_x
	local y

	if bar_position == "top" then
		y = wa.y + theme.margin_top + theme.offset_y
	else
		y = wa.y + wa.height - height - theme.margin_bottom + theme.offset_y
	end

	return {
		x = x,
		y = y,
		width = width,
		height = height,
		body_h = resolve_body_h(theme, entries, visible_entries),
	}
end

-- =========================================================================
-- Public API
-- =========================================================================

M.key_for_screen = key_for_screen
M.target_screens = target_screens
M.resolve_entries_count = resolve_entries_count
M.resolve_body_h = resolve_body_h
M.resolve_width = resolve_width
M.resolve_height_limit = resolve_height_limit
M.resolve_panel_h = resolve_panel_h
M.resolve_geometry = resolve_geometry

return M
