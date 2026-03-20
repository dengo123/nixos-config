-- ~/.config/awesome/shell/menu/init.lua
local awful = require("awful")

local function safe_require(path)
	local ok, mod = pcall(require, path)
	if ok then
		return mod
	end

	return nil
end

local Theme = safe_require("shell.menu.theme")
local Layout = safe_require("shell.menu.layout")
local Items = safe_require("shell.menu.items")
local Place = safe_require("shell.menu.lib.placement")
local Popup = safe_require("shell.menu.lib.popup")

assert(Theme and type(Theme) == "table", "shell.menu: theme fehlt")
assert(Layout and type(Layout) == "table", "shell.menu: layout fehlt")
assert(Items and type(Items) == "table", "shell.menu: items fehlt")
assert(Place and type(Place) == "table", "shell.menu: placement fehlt")
assert(Popup and type(Popup) == "table", "shell.menu: popup fehlt")

local Menu = {
	ui = nil,
	cfg = nil,
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function get_theme()
	local out = Theme.get()

	assert(type(out) == "table", "shell.menu: theme.get() lieferte kein table")

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

	if Theme and type(Theme.init) == "function" then
		Theme.init(Menu.cfg)
	end

	if Layout and type(Layout.init) == "function" then
		Layout.init(Menu.cfg)
	end
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
	return Popup.is_open()
end

function Menu.hide()
	Popup.hide()
end

return Menu
