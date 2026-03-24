-- ~/.config/awesome/shell/notify/center/popup.lua
local awful = require("awful")
local beautiful = require("beautiful")
local gears = require("gears")
local wibox = require("wibox")

local M = {}

local runtime = {
	root_buttons = nil,
	client_callback = nil,
}

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

local function disarm_close_guard()
	if runtime.root_buttons then
		pcall(function()
			root.buttons(runtime.root_buttons)
		end)
	end

	runtime.root_buttons = nil

	if runtime.client_callback then
		pcall(function()
			client.disconnect_signal("button::press", runtime.client_callback)
		end)
	end

	runtime.client_callback = nil
end

local function arm_close_guard(on_close)
	local function closer()
		if type(on_close) == "function" then
			on_close()
		end
	end

	runtime.root_buttons = root.buttons()

	local tmp = gears.table.join(
		runtime.root_buttons or {},
		awful.button({}, 1, closer),
		awful.button({}, 2, closer),
		awful.button({}, 3, closer)
	)

	root.buttons(tmp)

	local client_cb = function()
		closer()
	end

	runtime.client_callback = client_cb
	client.connect_signal("button::press", client_cb)
end

local function is_portrait(s)
	local wa = s.workarea
	return wa.height > wa.width
end

local function resolve_height_factor(theme, s)
	if is_portrait(s) then
		return tonumber(theme.height_factor_portrait) or tonumber(theme.height_factor) or 0.5
	end

	return tonumber(theme.height_factor_landscape) or tonumber(theme.height_factor) or 0.5
end

-- =========================================================================
-- Theme / Geometry
-- =========================================================================

function M.build_theme()
	local t = center_theme()

	return {
		width_factor = tonumber(t.width_factor),
		min_width = tonumber(t.min_width),
		max_width = tonumber(t.max_width),

		min_height = tonumber(t.min_height),
		max_height = tonumber(t.max_height),

		height_factor = tonumber(t.height_factor),
		height_factor_landscape = tonumber(t.height_factor_landscape),
		height_factor_portrait = tonumber(t.height_factor_portrait),

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
	local usable_h = wa.height - theme.margin_top - theme.margin_bottom
	local factor = resolve_height_factor(theme, s)

	local h = math.floor(usable_h * factor)

	local min_h = tonumber(theme.min_height) or 0
	local max_h = tonumber(theme.max_height)

	if h < min_h then
		h = min_h
	end

	if max_h and h > max_h then
		h = max_h
	end

	return h
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

-- =========================================================================
-- Public API
-- =========================================================================

function M.ensure(popups, key_for_screen, s)
	local key = key_for_screen(s)
	local popup = popups[key]

	if popup and popup.valid then
		return popup
	end

	popup = wibox({
		visible = false,
		ontop = true,
		screen = s,
		bg = "#00000000",
		type = "utility",
	})

	popups[key] = popup

	return popup
end

function M.rebuild(popup, build_panel)
	if not popup or not popup.valid then
		return false
	end

	local panel = build_panel()

	if not panel then
		popup.widget = nil
		popup.visible = false
		return false
	end

	popup:setup({
		{
			panel,
			halign = "fill",
			valign = "fill",
			widget = wibox.container.place,
		},
		layout = wibox.layout.align.vertical,
	})

	popup.widget = panel
	return true
end

function M.apply_geometry(popup, geo)
	if not popup or not popup.valid or not geo then
		return
	end

	if not popup.widget or geo.height <= 0 then
		popup.visible = false
		return
	end

	popup.width = geo.width
	popup.height = geo.height
	popup.x = geo.x
	popup.y = geo.y
end

function M.sync(args)
	local popups = args.popups or {}
	local screens = args.screens or {}
	local key_for_screen = args.key_for_screen
	local ensure_popup = args.ensure_popup
	local rebuild_popup = args.rebuild_popup
	local apply_geometry = args.apply_geometry

	local wanted = {}

	for _, s in ipairs(screens) do
		local key = key_for_screen(s)
		wanted[key] = true

		local popup = ensure_popup(s)
		local has_content = rebuild_popup(popup)

		if has_content then
			apply_geometry(popup)
		else
			popup.visible = false
		end
	end

	for key, popup in pairs(popups) do
		if not wanted[key] and popup and popup.valid then
			popup.visible = false
		end
	end
end

function M.set_visible(args, open)
	local popups = args.popups or {}
	local screens = args.screens or {}
	local key_for_screen = args.key_for_screen
	local sync_popups = args.sync_popups
	local close_on_click_outside = (args.close_on_click_outside == true)
	local on_close_request = args.on_close_request
	local on_open = args.on_open
	local on_close = args.on_close

	sync_popups()

	local wanted = {}
	for _, s in ipairs(screens) do
		wanted[key_for_screen(s)] = true
	end

	for key, popup in pairs(popups) do
		if popup and popup.valid then
			local has_widget = popup.widget ~= nil
			popup.visible = (open == true) and wanted[key] == true and has_widget
		end
	end

	if open == true then
		if close_on_click_outside then
			arm_close_guard(on_close_request)
		end

		if type(on_open) == "function" then
			on_open()
		end
	else
		disarm_close_guard()

		if type(on_close) == "function" then
			on_close()
		end
	end
end

function M.disarm()
	disarm_close_guard()
end

return M
