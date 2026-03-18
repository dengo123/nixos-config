-- shell/notify/center/init.lua
local awful = require("awful")
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
-- Helpers
-- =========================================================================

local function require_table(value, name)
	assert(type(value) == "table", "notify.center: " .. name .. " fehlt/ungültig")
	return value
end

local function notify_theme()
	return require_table(beautiful.notify, "beautiful.notify")
end

local function center_theme()
	local notify = notify_theme()
	return require_table(notify.center, "beautiful.notify.center")
end

local function bar_cfg()
	return runtime_cfg.bar or {}
end

local function screen_mode()
	return tostring(bar_cfg().show_notify or "primary"):lower()
end

local function target_screens()
	if screen_mode() == "all" then
		local out = {}
		for s in screen do
			table.insert(out, s)
		end
		return out
	end

	return { screen.primary or awful.screen.focused() }
end

local function key_for_screen(s)
	return tostring(s.index or 1)
end

local function entry_height()
	return tonumber(center_theme().entry_height) or 74
end

local function entry_spacing()
	return tonumber(center_theme().entry_spacing) or 10
end

local function list_pad_top()
	return tonumber(center_theme().list_pad_top) or 0
end

local function list_pad_bottom()
	return tonumber(center_theme().list_pad_bottom) or 0
end

local function resolve_content_height()
	local count = #History.list()

	if count <= 0 then
		return 0
	end

	return list_pad_top() + list_pad_bottom() + (count * entry_height()) + ((count - 1) * entry_spacing())
end

local function build_panel(max_height)
	return Widget.build(History.list(), max_height)
end

local function resolve_popup_geometry(popup)
	if not popup or not popup.screen then
		return nil
	end

	local theme = Geometry.build_theme()
	local width = Geometry.resolve_width(theme, popup.screen)
	local max_height = Geometry.resolve_max_height(theme, popup.screen)
	local content_height = resolve_content_height()
	local final_height = math.min(max_height, content_height)

	if final_height <= 0 then
		return {
			x = 0,
			y = 0,
			width = width,
			height = 0,
		}
	end

	return Geometry.resolve_position(theme, popup.screen, bar_cfg().position or "bottom", width, final_height)
end

-- =========================================================================
-- Popup
-- =========================================================================

local function ensure_popup(s)
	return Popup.ensure(popups, key_for_screen, s)
end

local function apply_geometry(popup)
	local geo = resolve_popup_geometry(popup)
	if not geo then
		return
	end

	Popup.apply_geometry(popup, geo)
end

local function rebuild_popup(popup)
	local geo = resolve_popup_geometry(popup)

	if not geo or geo.height <= 0 then
		return Popup.rebuild(popup, function()
			return nil
		end)
	end

	return Popup.rebuild(popup, function()
		return build_panel(geo.height)
	end)
end

local function sync_popups()
	Popup.sync({
		popups = popups,
		screens = target_screens(),
		key_for_screen = key_for_screen,
		ensure_popup = ensure_popup,
		rebuild_popup = rebuild_popup,
		apply_geometry = apply_geometry,
	})
end

local function set_visible(open)
	Popup.set_visible({
		popups = popups,
		screens = target_screens(),
		key_for_screen = key_for_screen,
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
