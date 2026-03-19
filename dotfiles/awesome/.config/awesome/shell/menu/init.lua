-- ~/.config/awesome/shell/menu/init.lua
local awful = require("awful")

local Items = require("shell.menu.items")
local Place = require("shell.menu.lib.placement")
local State = require("shell.menu.lib.state")
local CloseGuard = require("shell.menu.lib.close_guard")
local Popup = require("shell.menu.lib.popup")

local Menu = {}

-- =========================================================================
-- Helpers
-- =========================================================================

local function get_theme()
	local ui = State.get_context().ui or {}
	local theme = ui.theme and ui.theme.menu

	assert(theme and type(theme.get) == "function", "shell.menu: ui.theme.menu.get() fehlt")

	local out = theme.get()
	assert(type(out) == "table", "shell.menu: ui.theme.menu.get() lieferte kein table")

	assert(out.width, "shell.menu: theme.width fehlt")
	assert(out.height, "shell.menu: theme.height fehlt")
	assert(out.bg_normal, "shell.menu: theme.bg_normal fehlt")
	assert(out.fg_normal, "shell.menu: theme.fg_normal fehlt")
	assert(out.bg_focus, "shell.menu: theme.bg_focus fehlt")
	assert(out.fg_focus, "shell.menu: theme.fg_focus fehlt")
	assert(out.border_color, "shell.menu: theme.border_color fehlt")
	assert(out.border_width, "shell.menu: theme.border_width fehlt")
	assert(out.shape, "shell.menu: theme.shape fehlt")
	assert(out.submenu, "shell.menu: theme.submenu fehlt")

	return out
end

local function get_start_items()
	local items = Items.build_start(State.get_context())

	assert(type(items) == "table" and #items > 0, "shell.menu: Start-Items leer")

	return items
end

local function get_client_items(clients)
	local items = Items.build_clients(clients, State.get_context())

	assert(type(items) == "table" and #items > 0, "shell.menu: Tabs-Items leer")

	return items
end

local function ensure_closed()
	Popup.hide()
	CloseGuard.disarm()
end

local function tabs_enabled()
	local cfg = State.get_context().cfg or {}
	local menu_cfg = cfg.menu or {}

	return menu_cfg.tabs ~= false
end

local function open_menu(items, coords)
	ensure_closed()

	Popup.show({
		items = items,
		theme = get_theme(),
		coords = coords,
		on_closed = function()
			ensure_closed()
		end,
	})

	CloseGuard.arm(function()
		Menu.hide()
	end)
end

-- =========================================================================
-- Public API
-- =========================================================================

function Menu.init(args)
	args = args or {}
	State.set_context(args)
end

function Menu.get_start_items()
	return get_start_items()
end

function Menu.show_for_tabs_widget_with_clients_at(s, _widget, clients, anchor)
	if not tabs_enabled() then
		return
	end

	local items = get_client_items(clients)
	local x, y = Place.coords_for_tabs(s, anchor and anchor.x_left, #items)

	open_menu(items, {
		x = x,
		y = y,
	})
end

function Menu.show_for_start_widget(s, _widget)
	local items = get_start_items()
	local x, y = Place.coords_for_start(s, #items)

	open_menu(items, {
		x = x,
		y = y,
	})
end

function Menu.build_main()
	return awful.menu({
		items = get_start_items(),
		theme = get_theme(),
	})
end

function Menu.is_open()
	local menu = State.get_menu()

	return menu ~= nil and menu.wibox ~= nil and menu.wibox.valid and menu.wibox.visible == true
end

function Menu.hide()
	ensure_closed()
end

return Menu
