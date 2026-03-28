-- ~/.config/awesome/shell/launchers/lib/popup.lua
local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")

local Popup = {}

local runtime = {
	open_handles = {},
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function open_handles()
	return runtime.open_handles
end

local function nz(value, fallback)
	return value == nil and fallback or value
end

local function set_clients_visible(scr, on)
	local ok, clients = pcall(function()
		return client.get(scr)
	end)

	if ok and clients then
		for _, c in ipairs(clients) do
			c.hidden = not on
		end
	end
end

local function set_bars_visible(scr, on)
	for _, bar in pairs(scr.bars or {}) do
		if bar and bar.visible ~= nil then
			bar.visible = on
		end
	end
end

local function remove_handle(handle)
	for i = #open_handles(), 1, -1 do
		if open_handles()[i] == handle then
			table.remove(open_handles(), i)
			break
		end
	end
end

-- =========================================================================
-- Public API
-- =========================================================================

function Popup.close_all()
	for i = #open_handles(), 1, -1 do
		local handle = open_handles()[i]
		if handle and type(handle.close) == "function" then
			handle.close(true)
		end
	end
end

function Popup.show(form_widget, theme, opts)
	opts = opts or {}
	theme = theme or {}

	local s = opts.screen or awful.screen.focused()
	local width = assert(opts.width, "popup.show: width required")
	local height = assert(opts.height, "popup.show: height required")

	local placement_fn = opts.placement or awful.placement.centered
	local use_backdrop = nz(opts.use_backdrop, true)
	local close_on_backdrop = nz(opts.close_on_backdrop, false)

	local show_root_mode = opts.show_root
	if show_root_mode == nil then
		show_root_mode = "with_bars"
	end
	if show_root_mode == true then
		show_root_mode = "full"
	end

	local hide_clients = (show_root_mode == "with_bars") or (show_root_mode == "full")
	local hide_bars = (show_root_mode == "full")

	local backdrop = nil
	if use_backdrop then
		backdrop = wibox({
			screen = s,
			visible = true,
			ontop = true,
			type = "splash",
			bg = nz(theme.backdrop, "#00000066"),
		})

		backdrop.input_passthrough = false

		if show_root_mode == "with_bars" then
			local wa = s.workarea
			backdrop:geometry({
				x = wa.x,
				y = wa.y,
				width = wa.width,
				height = wa.height,
			})
		else
			backdrop:geometry(s.geometry)
		end
	end

	local popup = awful.popup({
		screen = s,
		ontop = true,
		visible = false,
		type = "dialog",
		bg = "#00000000",
		placement = placement_fn,
		shape = opts.shape,
		widget = wibox.widget({
			form_widget,
			strategy = "exact",
			width = width,
			height = height,
			widget = wibox.container.constraint,
		}),
	})

	local close_listeners = {}

	local handle = {
		popup = popup,
		backdrop = backdrop,
		screen = s,
		group = opts.group or "launchers",
		_is_open = false,
	}

	function handle.is_open()
		return handle._is_open == true
	end

	function handle.on_close(fn)
		if type(fn) == "function" then
			table.insert(close_listeners, fn)
		end
	end

	local on_tag_selected
	local on_screen_removed
	local on_screen_added
	local on_primary_changed

	local function disconnect_signals()
		if on_tag_selected then
			pcall(function()
				tag.disconnect_signal("property::selected", on_tag_selected)
			end)
			on_tag_selected = nil
		end

		if on_screen_removed then
			pcall(function()
				screen.disconnect_signal("removed", on_screen_removed)
			end)
			on_screen_removed = nil
		end

		if on_screen_added then
			pcall(function()
				screen.disconnect_signal("added", on_screen_added)
			end)
			on_screen_added = nil
		end

		if on_primary_changed then
			pcall(function()
				screen.disconnect_signal("primary_changed", on_primary_changed)
			end)
			on_primary_changed = nil
		end
	end

	local function fire_close_listeners()
		for _, fn in ipairs(close_listeners) do
			pcall(fn, handle)
		end
	end

	local function really_close()
		if not handle._is_open then
			return
		end

		handle._is_open = false

		if popup then
			popup.visible = false
			popup.widget = nil
		end

		if backdrop then
			backdrop.visible = false
		end

		if hide_clients then
			set_clients_visible(s, true)
		end

		if hide_bars then
			set_bars_visible(s, true)
		end

		remove_handle(handle)
		fire_close_listeners()
		awesome.emit_signal("ui::overlays_changed")
	end

	function handle.close()
		disconnect_signals()
		really_close()
	end

	if hide_clients then
		set_clients_visible(s, false)
	end

	if hide_bars then
		set_bars_visible(s, false)
	end

	popup.visible = true
	placement_fn(popup, { honor_workarea = true })
	handle._is_open = true
	awesome.emit_signal("ui::overlays_changed")

	if backdrop then
		if close_on_backdrop then
			backdrop:buttons(gears.table.join(awful.button({}, 1, function()
				handle.close()
			end)))
		else
			backdrop:buttons(
				gears.table.join(
					awful.button({}, 1, function() end),
					awful.button({}, 2, function() end),
					awful.button({}, 3, function() end)
				)
			)
		end
	end

	on_tag_selected = function()
		handle.close()
	end

	on_screen_removed = function()
		handle.close()
	end

	on_screen_added = function()
		handle.close()
	end

	on_primary_changed = function()
		handle.close()
	end

	tag.connect_signal("property::selected", on_tag_selected)
	screen.connect_signal("removed", on_screen_removed)
	screen.connect_signal("added", on_screen_added)
	screen.connect_signal("primary_changed", on_primary_changed)

	table.insert(open_handles(), handle)

	return handle
end

return Popup
