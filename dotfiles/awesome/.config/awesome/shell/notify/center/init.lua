-- ~/.config/awesome/shell/notify/center/init.lua
local beautiful = require("beautiful")

local CloseGuard = require("shell.notify.center.close_guard")
local Geometry = require("shell.notify.center.geometry")
local Popup = require("shell.notify.center.popup")
local Signals = require("shell.notify.center.signals")
local Widget = require("shell.notify.center.widget")
local History = require("shell.notify.history")

local M = {}

local popups = {}
local signals_ready = false
local runtime_cfg = {}

-- =========================================================================
-- Require
-- =========================================================================

local function require_number(value, name)
	local n = tonumber(value)
	assert(n ~= nil, "notify.center: " .. name .. " fehlt/ungültig")
	return n
end

local function require_string(value, name)
	assert(type(value) == "string" and value ~= "", "notify.center: " .. name .. " fehlt/ungültig")
	return value
end

local function require_table(value, name)
	assert(type(value) == "table", "notify.center: " .. name .. " fehlt/ungültig")
	return value
end

-- =========================================================================
-- Theme
-- =========================================================================

local function notify_theme()
	return require_table(beautiful.notify, "beautiful.notify")
end

local function center_theme()
	local notify = notify_theme()
	return require_table(notify.center, "beautiful.notify.center")
end

local function theme_bg()
	return require_string(notify_theme().bg, "beautiful.notify.bg")
end

local function theme_fg()
	return require_string(notify_theme().fg, "beautiful.notify.fg")
end

local function theme_border()
	return require_string(notify_theme().border, "beautiful.notify.border")
end

local function theme_radius()
	return require_number(notify_theme().radius, "beautiful.notify.radius")
end

local function theme_border_w()
	return require_number(notify_theme().border_w, "beautiful.notify.border_w")
end

local function center_width_factor()
	return require_number(center_theme().width_factor, "beautiful.notify.center.width_factor")
end

local function center_height_factor()
	return require_number(center_theme().height_factor, "beautiful.notify.center.height_factor")
end

local function center_min_width()
	return require_number(center_theme().min_width, "beautiful.notify.center.min_width")
end

local function center_min_height()
	return require_number(center_theme().min_height, "beautiful.notify.center.min_height")
end

local function center_max_width()
	return require_number(center_theme().max_width, "beautiful.notify.center.max_width")
end

local function center_max_height()
	return require_number(center_theme().max_height, "beautiful.notify.center.max_height")
end

local function center_offset_x()
	return require_number(center_theme().offset_x, "beautiful.notify.center.offset_x")
end

local function center_offset_y()
	return require_number(center_theme().offset_y, "beautiful.notify.center.offset_y")
end

local function center_margin_top()
	return require_number(center_theme().margin_top, "beautiful.notify.center.margin_top")
end

local function center_margin_right()
	return require_number(center_theme().margin_right, "beautiful.notify.center.margin_right")
end

local function center_margin_bottom()
	return require_number(center_theme().margin_bottom, "beautiful.notify.center.margin_bottom")
end

local function center_spacing()
	return require_number(center_theme().spacing, "beautiful.notify.center.spacing")
end

local function center_padding()
	return require_number(center_theme().padding, "beautiful.notify.center.padding")
end

local function center_header_h()
	return require_number(center_theme().header_height, "beautiful.notify.center.header_height")
end

local function center_entry_h()
	return require_number(center_theme().entry_height, "beautiful.notify.center.entry_height")
end

local function center_empty_h()
	return require_number(center_theme().empty_height, "beautiful.notify.center.empty_height")
end

local function center_panel_radius()
	local t = center_theme()
	return require_number(t.panel_radius or theme_radius(), "beautiful.notify.center.panel_radius")
end

local function center_panel_border_w()
	local t = center_theme()
	return require_number(t.panel_border_width or theme_border_w(), "beautiful.notify.center.panel_border_width")
end

local function center_panel_border()
	local t = center_theme()
	return require_string(t.panel_border or theme_border(), "beautiful.notify.center.panel_border")
end

local function center_panel_bg()
	local t = center_theme()
	return require_string(t.panel_bg or theme_bg(), "beautiful.notify.center.panel_bg")
end

local function center_header_bg()
	local t = center_theme()
	return require_string(t.header_bg or theme_bg(), "beautiful.notify.center.header_bg")
end

local function center_header_fg()
	local t = center_theme()
	return require_string(t.header_fg or theme_fg(), "beautiful.notify.center.header_fg")
end

local function center_body_bg()
	local t = center_theme()
	return require_string(t.body_bg or center_panel_bg(), "beautiful.notify.center.body_bg")
end

local function center_body_fg()
	local t = center_theme()
	return require_string(t.body_fg or theme_fg(), "beautiful.notify.center.body_fg")
end

local function build_widget_theme()
	return {
		padding = center_padding(),
		spacing = center_spacing(),
		header_h = center_header_h(),
		entry_h = center_entry_h(),
		header_bg = center_header_bg(),
		header_fg = center_header_fg(),
		body_bg = center_body_bg(),
		body_fg = center_body_fg(),
		panel_radius = center_panel_radius(),
		panel_border_w = center_panel_border_w(),
		panel_border = center_panel_border(),
		panel_bg = center_panel_bg(),
	}
end

local function build_geometry_theme()
	return {
		width_factor = center_width_factor(),
		height_factor = center_height_factor(),
		min_width = center_min_width(),
		min_height = center_min_height(),
		max_width = center_max_width(),
		max_height = center_max_height(),
		offset_x = center_offset_x(),
		offset_y = center_offset_y(),
		margin_top = center_margin_top(),
		margin_right = center_margin_right(),
		margin_bottom = center_margin_bottom(),
		header_h = center_header_h(),
		entry_h = center_entry_h(),
		empty_h = center_empty_h(),
		spacing = center_spacing(),
	}
end

-- =========================================================================
-- Runtime
-- =========================================================================

local function notify_cfg()
	return runtime_cfg.notify or {}
end

local function center_visible_entries()
	return tonumber(notify_cfg().visible_entries) or 5
end

-- =========================================================================
-- Widgets
-- =========================================================================

local function build_panel()
	local entries = History.list()
	local visible_entries = center_visible_entries()
	local geo_theme = build_geometry_theme()

	return Widget.build_panel({
		theme = build_widget_theme(),
		entries = entries,
		visible_entries = visible_entries,
		body_h = Geometry.resolve_body_h(geo_theme, entries, visible_entries),
	})
end

-- =========================================================================
-- Popup
-- =========================================================================

local function resolve_popup_geometry(popup)
	local geo = Geometry.resolve_geometry(
		runtime_cfg,
		build_geometry_theme(),
		popup.screen,
		History.list(),
		center_visible_entries()
	)

	geo.radius = center_panel_radius()

	return geo
end

local function ensure_popup(s)
	return Popup.ensure(popups, Geometry.key_for_screen, s)
end

local function apply_geometry(popup)
	Popup.apply_geometry(popup, resolve_popup_geometry(popup))
end

local function rebuild_popup(popup)
	Popup.rebuild(popup, build_panel, resolve_popup_geometry(popup))
end

local function sync_popups()
	Popup.sync({
		popups = popups,
		target_screens = function()
			return Geometry.target_screens(runtime_cfg)
		end,
		key_for_screen = Geometry.key_for_screen,
		ensure_popup = ensure_popup,
		rebuild_popup = rebuild_popup,
		apply_geometry = apply_geometry,
	})
end

local function set_visible(open)
	Popup.set_visible({
		popups = popups,
		target_screens = function()
			return Geometry.target_screens(runtime_cfg)
		end,
		key_for_screen = Geometry.key_for_screen,
		sync_popups = sync_popups,
		on_open = function()
			CloseGuard.arm(function()
				awesome.emit_signal("notify::close_center")
			end)
		end,
		on_close = function()
			CloseGuard.disarm()
		end,
	}, open)
end

-- =========================================================================
-- Signals
-- =========================================================================

local function each_popup(fn)
	for _, popup in pairs(popups) do
		fn(popup)
	end
end

local function is_signals_ready()
	return signals_ready
end

local function set_signals_ready(value)
	signals_ready = (value == true)
end

local function register_signals()
	Signals.register({
		is_ready = is_signals_ready,
		set_ready = set_signals_ready,
		set_visible = set_visible,
		each_popup = each_popup,
		rebuild_popup = rebuild_popup,
		apply_geometry = apply_geometry,
		sync_popups = sync_popups,
	})
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(cfg)
	runtime_cfg = cfg or {}
	sync_popups()
	register_signals()
end

return M
