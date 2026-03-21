-- ~/.config/awesome/shell/notify/center/init.lua
local awful = require("awful")
local beautiful = require("beautiful")

local function safe_require(path)
	local ok, mod = pcall(require, path)
	if ok then
		return mod
	end

	return nil
end

local M = {
	api = {},
}

local popups = {}
local signals_ready = false
local runtime_cfg = {}

-- =========================================================================
-- Helpers
-- =========================================================================

local function api()
	return M.api or {}
end

local function mod(name)
	return api()[name]
end

local function notify_cfg()
	return runtime_cfg.notify or {}
end

local function center_cfg()
	return notify_cfg().center or {}
end

local function notify_theme()
	return beautiful.notify or {}
end

local function center_theme()
	local notify = notify_theme()
	return notify.center or {}
end

local function bar_cfg()
	return runtime_cfg.bar or {}
end

local function screen_mode()
	return string.lower(tostring(bar_cfg().show_notify or "primary"))
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
	return primary and { primary } or {}
end

local function key_for_screen(s)
	return tostring(tonumber(s.index) or 0)
end

local function list_pad_top()
	return tonumber(center_theme().list_pad_top) or 0
end

local function list_pad_bottom()
	return tonumber(center_theme().list_pad_bottom) or 0
end

local function entry_height(entry)
	local theme = center_theme()
	local base = tonumber(theme.entry_height) or 0
	local with_actions = tonumber(theme.entry_height_with_actions) or base

	if type(entry.actions) == "table" and #entry.actions > 0 then
		return with_actions
	end

	return base
end

local function entry_spacing()
	return tonumber(center_theme().entry_spacing) or 0
end

local function resolve_content_height()
	local History = mod("history")
	local entries = (History and History.list and History.list()) or {}
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
	local Widget = mod("widget")
	local History = mod("history")
	local entries = (History and History.list and History.list()) or {}

	if Widget and type(Widget.build) == "function" then
		return Widget.build(entries, max_height, runtime_cfg)
	end

	return nil
end

local function resolve_popup_geometry(popup)
	local Geometry = mod("geometry")

	if not (popup and popup.screen and Geometry) then
		return nil
	end

	local theme = Geometry.build_theme and Geometry.build_theme() or {}
	local width = Geometry.resolve_width and Geometry.resolve_width(theme, popup.screen) or 0
	local max_height = Geometry.resolve_max_height and Geometry.resolve_max_height(theme, popup.screen) or 0
	local content_height = resolve_content_height()
	local final_height = math.min(max_height, content_height)
	local bar_position = tostring(bar_cfg().position or "bottom")

	if final_height <= 0 then
		return {
			x = 0,
			y = 0,
			width = width,
			height = 0,
		}
	end

	if Geometry.resolve_position then
		return Geometry.resolve_position(theme, popup.screen, bar_position, width, final_height)
	end

	return nil
end

-- =========================================================================
-- Popup
-- =========================================================================

local function ensure_popup(s)
	local Popup = mod("popup")
	if Popup and type(Popup.ensure) == "function" then
		return Popup.ensure(popups, key_for_screen, s)
	end

	return nil
end

local function apply_geometry(popup)
	local Popup = mod("popup")
	local geo = resolve_popup_geometry(popup)

	if not (Popup and geo) then
		return
	end

	Popup.apply_geometry(popup, geo)
end

local function rebuild_popup(popup)
	local Popup = mod("popup")
	local geo = resolve_popup_geometry(popup)

	if not Popup then
		return nil
	end

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
	local Popup = mod("popup")
	if not (Popup and type(Popup.sync) == "function") then
		return
	end

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
	local Popup = mod("popup")
	if not (Popup and type(Popup.set_visible) == "function") then
		return
	end

	Popup.set_visible({
		popups = popups,
		screens = target_screens(),
		key_for_screen = key_for_screen,
		sync_popups = sync_popups,
		close_on_click_outside = (center_cfg().close_on_click_outside == true),
		on_close_request = function()
			awesome.emit_signal("notify::close_center")
		end,
		on_open = function() end,
		on_close = function() end,
	}, open)
end

local function register_escape_signal()
	if center_cfg().close_on_escape ~= true then
		return
	end
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
	local Signals = mod("signals")
	if not (Signals and type(Signals.register) == "function") then
		return
	end

	Signals.register({
		cfg = runtime_cfg,
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

function M.init(args)
	args = args or {}

	runtime_cfg = args.cfg or args or {}

	M.api = {
		ui = args.ui or {},
		geometry = safe_require("shell.notify.center.geometry"),
		popup = safe_require("shell.notify.center.popup"),
		signals = safe_require("shell.notify.center.signals"),
		widget = safe_require("shell.notify.center.widget"),
		history = safe_require("shell.notify.history"),
	}

	sync_popups()
	register_escape_signal()
	register_signals()

	return M
end

return M
