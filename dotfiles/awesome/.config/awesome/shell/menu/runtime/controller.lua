-- ~/.config/awesome/shell/menu/runtime/controller.lua
local awful = require("awful")

local M = {}

local runtime = {
	cfg = {},
	ui = {},
	items = nil,
	applications = nil,
	launchers = nil,
	layout = nil,
	popup = nil,
	theme = nil,
	placement = nil,
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function cfg()
	return runtime.cfg or {}
end

local function ui()
	return runtime.ui or {}
end

local function items_mod()
	return runtime.items
end

local function popup_mod()
	return runtime.popup
end

local function theme_mod()
	return runtime.theme
end

local function placement_mod()
	return runtime.placement
end

local function menu_cfg()
	return cfg().menu or {}
end

local function tabs_enabled()
	return menu_cfg().tabs ~= false
end

local function get_theme()
	local Theme = theme_mod()
	return (Theme and type(Theme.get) == "function" and Theme.get()) or {}
end

local function build_items_context()
	return {
		cfg = cfg(),
		ui = ui(),
		applications = runtime.applications,
		launchers = runtime.launchers,
	}
end

local function get_start_items()
	local Items = items_mod()
	return (Items and type(Items.build_start) == "function" and Items.build_start(build_items_context())) or {}
end

local function get_client_items(clients)
	local Items = items_mod()
	return (Items and type(Items.build_clients) == "function" and Items.build_clients(clients, build_items_context()))
		or {}
end

local function open_menu(items, coords)
	local Popup = popup_mod()

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
	local Placement = placement_mod()
	local x, y = 0, 0

	if Placement and type(Placement.coords_for_tabs) == "function" then
		x, y = Placement.coords_for_tabs(s, anchor_x_left, item_count)
	end

	return x, y
end

local function coords_for_start(s, item_count)
	local Placement = placement_mod()
	local x, y = 0, 0

	if Placement and type(Placement.coords_for_start) == "function" then
		x, y = Placement.coords_for_start(s, item_count)
	end

	return x, y
end

local function coords_for_keybind(s, item_count)
	local Placement = placement_mod()
	local x, y = 0, 0

	if Placement and type(Placement.coords_for_keybind) == "function" then
		x, y = Placement.coords_for_keybind(s, item_count)
	end

	return x, y
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(opts)
	opts = opts or {}

	runtime.cfg = opts.cfg or runtime.cfg
	runtime.ui = opts.ui or runtime.ui
	runtime.items = opts.items or runtime.items
	runtime.applications = opts.applications or runtime.applications
	runtime.launchers = opts.launchers or runtime.launchers
	runtime.layout = opts.layout or runtime.layout
	runtime.popup = opts.popup or runtime.popup
	runtime.theme = opts.theme or runtime.theme
	runtime.placement = opts.placement or runtime.placement

	return M
end

function M.get_start_items()
	return get_start_items()
end

function M.show_for_tabs_widget_with_clients_at(s, clients, anchor)
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

function M.show_for_start_widget(s)
	local items = get_start_items()
	local x, y = coords_for_start(s, #items)

	open_menu(items, {
		x = x,
		y = y,
	})
end

function M.build_main()
	return awful.menu({
		items = get_start_items(),
		theme = get_theme(),
	})
end

function M.is_open()
	local Popup = popup_mod()
	return Popup and type(Popup.is_open) == "function" and Popup.is_open() or false
end

function M.hide()
	local Popup = popup_mod()
	if Popup and type(Popup.hide) == "function" then
		Popup.hide()
	end
end

function M.show_for_keybind(s)
	local items = get_start_items()
	local x, y = coords_for_keybind(s, #items)

	open_menu(items, {
		x = x,
		y = y,
	})
end

return M
