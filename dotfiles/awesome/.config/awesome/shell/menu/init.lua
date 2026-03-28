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
	applications = nil,
	runtime = {},
	ui = {},
	items = nil,
}

local runtime = {
	cfg = {},
	ui = {},
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

-- =========================================================================
-- Public API
-- =========================================================================

function Menu.init(opts)
	opts = opts or {}

	runtime.cfg = opts.cfg or runtime.cfg
	runtime.ui = opts.ui or runtime.ui

	Menu.applications = safe_require("shell.menu.applications")
	Menu.items = safe_require("shell.menu.items")

	Menu.runtime = {
		controller = safe_require("shell.menu.runtime.controller"),
		placement = safe_require("shell.menu.runtime.placement"),
		signals = safe_require("shell.menu.runtime.signals"),
	}

	Menu.ui = {
		layout = safe_require("shell.menu.ui.layout"),
		popup = safe_require("shell.menu.ui.popup"),
		theme = safe_require("shell.menu.ui.theme"),
	}

	local Theme = Menu.ui.theme
	if Theme and type(Theme.init) == "function" then
		Theme.init({
			ui = ui(),
		})
	end

	local Layout = Menu.ui.layout
	if Layout and type(Layout.init) == "function" then
		Layout.init({
			cfg = cfg(),
			ui = ui(),
		})
	end

	local Placement = Menu.runtime.placement
	if Placement and type(Placement.init) == "function" then
		Placement.init({
			layout = Menu.ui.layout,
		})
	end

	local Apps = Menu.applications
	if Apps and type(Apps.init) == "function" then
		Apps.init({
			cfg = cfg(),
			ui = ui(),
		})
	end

	if Apps and type(Apps.load) == "function" then
		Apps.load()
	end

	local Controller = Menu.runtime.controller
	if Controller and type(Controller.init) == "function" then
		Controller.init({
			cfg = cfg(),
			ui = ui(),
			items = Menu.items,
			applications = Menu.applications,
			launchers = opts.launchers,
			layout = Menu.ui.layout,
			popup = Menu.ui.popup,
			theme = Menu.ui.theme,
			placement = Menu.runtime.placement,
		})
	end

	local Signals = Menu.runtime.signals
	if Signals and type(Signals.init) == "function" then
		Signals.init({
			menu = Menu,
		})
	end

	return Menu
end

function Menu.get_start_items()
	local Controller = Menu.runtime.controller
	if Controller and type(Controller.get_start_items) == "function" then
		return Controller.get_start_items()
	end

	return {}
end

function Menu.show_for_tabs_widget_with_clients_at(s, _widget, clients, anchor)
	local Controller = Menu.runtime.controller
	if Controller and type(Controller.show_for_tabs_widget_with_clients_at) == "function" then
		Controller.show_for_tabs_widget_with_clients_at(s, clients, anchor)
	end
end

function Menu.show_for_start_widget(s, _widget)
	local Controller = Menu.runtime.controller
	if Controller and type(Controller.show_for_start_widget) == "function" then
		Controller.show_for_start_widget(s)
	end
end

function Menu.build_main()
	local Controller = Menu.runtime.controller
	if Controller and type(Controller.build_main) == "function" then
		return Controller.build_main()
	end

	return awful.menu({ items = {} })
end

function Menu.is_open()
	local Controller = Menu.runtime.controller
	return Controller and type(Controller.is_open) == "function" and Controller.is_open() or false
end

function Menu.hide()
	local Controller = Menu.runtime.controller
	if Controller and type(Controller.hide) == "function" then
		Controller.hide()
	end
end

function Menu.show_for_keybind(s)
	local Controller = Menu.runtime.controller
	if Controller and type(Controller.show_for_keybind) == "function" then
		Controller.show_for_keybind(s)
	end
end

return Menu
