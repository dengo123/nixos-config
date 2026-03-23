-- ~/.config/awesome/shell/menu/init.lua
local awful = require("awful")

local function safe_require(path)
	local ok, mod = pcall(require, path)
	if ok then
		return mod
	end

	return nil
end

local Menu = {
	ui = nil,
	cfg = nil,
	api = {},
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function api()
	return Menu.api or {}
end

local function theme_api()
	return api().theme
end

local function layout_api()
	return api().layout
end

local function items_api()
	return api().items
end

local function placement_api()
	return api().placement
end

local function popup_api()
	return api().popup
end

local function ui_api()
	return api().ui or {}
end

local function get_theme()
	local Theme = theme_api()
	return (Theme and Theme.get and Theme.get()) or {}
end

local function get_context()
	return {
		ui = Menu.ui or {},
		cfg = Menu.cfg or {},
		api = api(),
	}
end

local function get_start_items()
	local Items = items_api()
	return (Items and Items.build_start and Items.build_start(get_context())) or {}
end

local function get_client_items(clients)
	local Items = items_api()
	return (Items and Items.build_clients and Items.build_clients(clients, get_context())) or {}
end

local function tabs_enabled()
	local cfg = Menu.cfg or {}
	local menu_cfg = cfg.menu or {}

	return menu_cfg.tabs ~= false
end

local function open_menu(items, coords)
	local Popup = popup_api()

	if Popup and type(Popup.show) == "function" then
		Popup.show({
			items = items,
			theme = get_theme(),
			coords = coords,
			on_closed = function() end,
		})
	end
end

-- =========================================================================
-- Public API
-- =========================================================================

function Menu.init(args)
	args = args or {}

	Menu.ui = args.ui or {}
	Menu.cfg = args.cfg or {}

	Menu.api = {
		ui = args.ui or {},
		theme = safe_require("shell.menu.theme"),
		layout = safe_require("shell.menu.layout"),
		items = safe_require("shell.menu.items"),
		placement = safe_require("shell.menu.lib.placement"),
		popup = safe_require("shell.menu.lib.popup"),
	}

	local Theme = theme_api()
	local Layout = layout_api()
	local Place = placement_api()
	local _ui = ui_api()

	if not (_ui and _ui.theme and _ui.theme.colors and _ui.theme.fonts) then
		local UI = require("ui")
		local ui_mod = UI.init({ cfg = Menu.cfg })
		_ui = ui_mod.get()
		Menu.ui = _ui
		Menu.api.ui = _ui
	end

	if Theme and type(Theme.init) == "function" then
		Theme.init({
			cfg = Menu.cfg,
			ui = _ui,
		})
	end

	if Layout and type(Layout.init) == "function" then
		Layout.init({
			cfg = Menu.cfg,
			ui = _ui,
		})
	end

	if Place and type(Place.init) == "function" then
		Place.init({
			api = {
				layout = Layout,
				ui = _ui,
			},
		})
	end
end

function Menu.get_start_items()
	return get_start_items()
end

function Menu.show_for_tabs_widget_with_clients_at(s, _widget, clients, anchor)
	if not tabs_enabled() then
		return
	end

	local Place = placement_api()
	local items = get_client_items(clients)
	local x, y = 0, 0

	if Place and type(Place.coords_for_tabs) == "function" then
		x, y = Place.coords_for_tabs(s, anchor and anchor.x_left, #items)
	end

	open_menu(items, {
		x = x,
		y = y,
	})
end

function Menu.show_for_start_widget(s, _widget)
	local Place = placement_api()
	local items = get_start_items()
	local x, y = 0, 0

	if Place and type(Place.coords_for_start) == "function" then
		x, y = Place.coords_for_start(s, #items)
	end

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
	local Popup = popup_api()
	return Popup and type(Popup.is_open) == "function" and Popup.is_open() or false
end

function Menu.hide()
	local Popup = popup_api()
	if Popup and type(Popup.hide) == "function" then
		Popup.hide()
	end
end

return Menu
