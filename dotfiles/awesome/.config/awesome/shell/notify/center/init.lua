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

local function require_number(value, name)
	local n = tonumber(value)
	assert(n ~= nil, "notify.center: " .. name .. " fehlt/ungültig")
	return n
end

local function require_string(value, name)
	assert(type(value) == "string" and value ~= "", "notify.center: " .. name .. " fehlt/ungültig")
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
	return require_table(runtime_cfg.bar, "cfg.bar")
end

local function screen_mode()
	return string.lower(require_string(bar_cfg().show_notify, "cfg.bar.show_notify"))
end

local function target_screens()
	if screen_mode() == "all" then
		local out = {}

		for s in screen do
			table.insert(out, s)
		end

		return out
	end

	local primary = screen.primary or awful.screen.focused()
	assert(primary ~= nil, "notify.center: kein Zielscreen verfügbar")

	return { primary }
end

local function key_for_screen(s)
	local index = tonumber(s.index)
	assert(index ~= nil, "notify.center: screen.index fehlt/ungültig")
	return tostring(index)
end

local function list_pad_top()
	return require_number(center_theme().list_pad_top, "beautiful.notify.center.list_pad_top")
end

local function list_pad_bottom()
	return require_number(center_theme().list_pad_bottom, "beautiful.notify.center.list_pad_bottom")
end

local function entry_height(entry)
	local theme = center_theme()
	local base = require_number(theme.entry_height, "beautiful.notify.center.entry_height")
	local with_actions =
		require_number(theme.entry_height_with_actions, "beautiful.notify.center.entry_height_with_actions")

	if type(entry.actions) == "table" and #entry.actions > 0 then
		return with_actions
	end

	return base
end

local function entry_spacing()
	return require_number(center_theme().entry_spacing, "beautiful.notify.center.entry_spacing")
end

local function resolve_content_height()
	local entries = History.list()
	local count = #entries

	if count <= 0 then
		return 0
	end

	local height = list_pad_top() + list_pad_bottom()

	for i, entry in ipairs(entries) do
		height = height + entry_height(entry)

		if i < count then
			height = height + entry_spacing()
		end
	end

	return height
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
	local bar_position = require_string(bar_cfg().position, "cfg.bar.position")

	if final_height <= 0 then
		return {
			x = 0,
			y = 0,
			width = width,
			height = 0,
		}
	end

	return Geometry.resolve_position(theme, popup.screen, bar_position, width, final_height)
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
	runtime_cfg = require_table(cfg, "cfg")
	sync_popups()
	register_signals()
end

return M
