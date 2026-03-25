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

local signals_ready = false

-- =========================================================================
-- Helpers
-- =========================================================================

local function api()
	return Menu.api or {}
end

local function mod(name)
	return api()[name]
end

local function get_context()
	return {
		ui = Menu.ui or {},
		cfg = Menu.cfg or {},
		api = api(),
	}
end

local function menu_cfg()
	return (Menu.cfg or {}).menu or {}
end

local function tabs_enabled()
	return menu_cfg().tabs ~= false
end

local function ensure_ui()
	local _ui = Menu.ui or {}

	if _ui and _ui.theme and _ui.theme.colors and _ui.theme.fonts then
		return _ui
	end

	local UI = require("ui")
	local ui_mod = UI.init({ cfg = Menu.cfg })
	_ui = ui_mod.get()

	Menu.ui = _ui
	Menu.api.ui = _ui

	return _ui
end

local function get_theme()
	local Theme = mod("theme")
	return (Theme and type(Theme.get) == "function" and Theme.get()) or {}
end

local function get_start_items()
	local Items = mod("items")
	return (Items and type(Items.build_start) == "function" and Items.build_start(get_context())) or {}
end

local function get_client_items(clients)
	local Items = mod("items")
	return (Items and type(Items.build_clients) == "function" and Items.build_clients(clients, get_context())) or {}
end

local function open_menu(items, coords)
	local Popup = mod("popup")

	if Popup and type(Popup.show) == "function" then
		Popup.show({
			items = items,
			theme = get_theme(),
			coords = coords,
			on_closed = function() end,
		})
	end
end

local function coords_for_tabs(s, anchor_x_left, item_count)
	local Place = mod("placement")
	local x, y = 0, 0

	if Place and type(Place.coords_for_tabs) == "function" then
		x, y = Place.coords_for_tabs(s, anchor_x_left, item_count)
	end

	return x, y
end

local function coords_for_start(s, item_count)
	local Place = mod("placement")
	local x, y = 0, 0

	if Place and type(Place.coords_for_start) == "function" then
		x, y = Place.coords_for_start(s, item_count)
	end

	return x, y
end

local function register_signals()
	if signals_ready then
		return
	end

	signals_ready = true

	awesome.connect_signal("menu::toggle", function()
		if Menu.is_open() then
			Menu.hide()
		else
			Menu.show_for_keybind(awful.screen.focused())
		end
	end)

	awesome.connect_signal("menu::open_start", function()
		if not Menu.is_open() then
			Menu.show_for_keybind(awful.screen.focused())
		end
	end)

	awesome.connect_signal("menu::close", function()
		if Menu.is_open() then
			Menu.hide()
		end
	end)
end

local function coords_for_keybind(s, item_count)
	local Place = mod("placement")
	local x, y = 0, 0

	if Place and type(Place.coords_for_keybind) == "function" then
		x, y = Place.coords_for_keybind(s, item_count)
	end

	return x, y
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
		layout = safe_require("shell.menu.lib.layout"),
		items = safe_require("shell.menu.items"),
		applications = safe_require("shell.menu.applications"),
		placement = safe_require("shell.menu.lib.placement"),
		popup = safe_require("shell.menu.lib.popup"),
	}

	local _ui = ensure_ui()

	local Theme = mod("theme")
	if Theme and type(Theme.init) == "function" then
		Theme.init({
			cfg = Menu.cfg,
			ui = _ui,
		})
	end

	local Layout = mod("layout")
	if Layout and type(Layout.init) == "function" then
		Layout.init({
			cfg = Menu.cfg,
			ui = _ui,
		})
	end

	local Place = mod("placement")
	if Place and type(Place.init) == "function" then
		Place.init({
			api = {
				layout = Layout,
				ui = _ui,
			},
		})
	end

	local Apps = mod("applications")
	if Apps and type(Apps.init) == "function" then
		Apps.init({
			cfg = Menu.cfg,
			ui = _ui,
		})
	end

	if Apps and type(Apps.load) == "function" then
		Apps.load()
	end

	register_signals()
end

function Menu.get_start_items()
	return get_start_items()
end

function Menu.show_for_tabs_widget_with_clients_at(s, _widget, clients, anchor)
	if not tabs_enabled() then
		return
	end

	local items = get_client_items(clients)
	local x, y = coords_for_tabs(s, anchor and anchor.x_left, #items)

	open_menu(items, {
		x = x,
		y = y,
	})
end

function Menu.show_for_start_widget(s, _widget)
	local items = get_start_items()
	local x, y = coords_for_start(s, #items)

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
	local Popup = mod("popup")
	return Popup and type(Popup.is_open) == "function" and Popup.is_open() or false
end

function Menu.hide()
	local Popup = mod("popup")
	if Popup and type(Popup.hide) == "function" then
		Popup.hide()
	end
end

function Menu.show_for_keybind(s)
	local items = get_start_items()
	local x, y = coords_for_keybind(s, #items)

	open_menu(items, {
		x = x,
		y = y,
	})
end

return Menu
