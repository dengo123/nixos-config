-- ~/.config/awesome/shell/menu/init.lua
local awful = require("awful")

local Items = require("shell.menu.lib.items")
local Place = require("shell.menu.lib.placement")
local Popup = require("shell.menu.lib.popup")

local Menu = {
	ui = nil,
	cfg = nil,
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function get_theme()
	local ui = Menu.ui or {}
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

local function get_context()
	return {
		ui = Menu.ui or {},
		cfg = Menu.cfg or {},
	}
end

local function get_start_items()
	local items = Items.build_start(get_context())

	assert(type(items) == "table" and #items > 0, "shell.menu: Start-Items leer")

	return items
end

local function get_client_items(clients)
	local items = Items.build_clients(clients, get_context())

	assert(type(items) == "table" and #items > 0, "shell.menu: Tabs-Items leer")

	return items
end

local function tabs_enabled()
	local cfg = Menu.cfg or {}
	local menu_cfg = cfg.menu or {}

	return menu_cfg.tabs ~= false
end

local function open_menu(items, coords)
	Popup.show({
		items = items,
		theme = get_theme(),
		coords = coords,
		on_closed = function() end,
	})
end

-- =========================================================================
-- Public API
-- =========================================================================

function Menu.init(args)
	args = args or {}

	Menu.ui = args.ui or {}
	Menu.cfg = args.cfg or {}
end

function Menu.get_start_items()
	return get_start_items()
end

function Menu.show_for_tabs_widget_with_clients_at(s, _widget, clients, anchor)
	if not tabs_enabled() then
		return
	end

	local items = get_client_items(clients)
	local x, y = Place.coords_for_tabs(Menu.ui, s, anchor and anchor.x_left, #items)

	open_menu(items, {
		x = x,
		y = y,
	})
end

function Menu.show_for_start_widget(s, _widget)
	local items = get_start_items()
	local x, y = Place.coords_for_start(Menu.ui, s, #items)

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
	return Popup.is_open()
end

function Menu.hide()
	Popup.hide()
end

return Menu
