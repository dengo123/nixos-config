-- ~/.config/awesome/shell/menu/init.lua
local awful = require("awful")
local beautiful = require("beautiful")
local gears = require("gears")

local Items = require("shell.menu.items")
local Place = require("shell.menu.placement")
local State = require("shell.menu.lib.state")
local Keygrab = require("shell.menu.lib.keygrab")

local Menu = {}

-- ---------------------------------------------------------------------------
-- Theme-Block (bewusst strikt: Keys müssen gesetzt sein)
local function theme_block()
	assert(beautiful.menu_width, "theme: beautiful.menu_width fehlt")
	assert(beautiful.menu_height, "theme: beautiful.menu_height fehlt")
	assert(beautiful.menu_bg_normal, "theme: beautiful.menu_bg_normal fehlt")
	assert(beautiful.menu_fg_normal, "theme: beautiful.menu_fg_normal fehlt")
	assert(beautiful.menu_bg_focus, "theme: beautiful.menu_bg_focus fehlt")
	assert(beautiful.menu_fg_focus, "theme: beautiful.menu_fg_focus fehlt")
	assert(beautiful.menu_border_color, "theme: beautiful.menu_border_color fehlt")
	assert(beautiful.menu_border_width, "theme: beautiful.menu_border_width fehlt")
	assert(beautiful.menu_shape, "theme: beautiful.menu_shape fehlt")
	assert(beautiful.menu_submenu, "theme: beautiful.menu_submenu fehlt")

	return {
		width = beautiful.menu_width,
		height = beautiful.menu_height,
		bg_normal = beautiful.menu_bg_normal,
		fg_normal = beautiful.menu_fg_normal,
		bg_focus = beautiful.menu_bg_focus,
		fg_focus = beautiful.menu_fg_focus,
		border_color = beautiful.menu_border_color,
		border_width = beautiful.menu_border_width,
		shape = beautiful.menu_shape,
		submenu = beautiful.menu_submenu,
	}
end

-- ---------------------------------------------------------------------------
-- Kontext & Items
function Menu.init(args)
	State.set_context(args or {})
end

function Menu.get_start_items()
	local ctx = State.get_context()

	assert(Items and type(Items.build_start) == "function", "shell.menu: items.build_start(ctx) fehlt")
	local items = Items.build_start(ctx)
	assert(type(items) == "table" and #items > 0, "shell.menu: Start-Items leer")
	return items
end

local function get_client_items(clients)
	local ctx = State.get_context()

	assert(Items and type(Items.build_clients) == "function", "shell.menu: items.build_clients(clients, ctx) fehlt")
	local items = Items.build_clients(clients, ctx)
	assert(type(items) == "table" and #items > 0, "shell.menu: Tabs-Items leer")
	return items
end

-- Click-to-Close: temporäre Root-Buttons + Client-Klick
local function arm_click_to_close()
	State.set_root_buttons(root.buttons())

	local closer = function()
		Menu.hide()
	end

	local tmp = gears.table.join(
		State.get_root_buttons() or {},
		awful.button({}, 1, closer),
		awful.button({}, 2, closer),
		awful.button({}, 3, closer)
	)

	root.buttons(tmp)

	State.set_client_callback(function(_c, _x, _y, _button)
		Menu.hide()
	end)

	client.connect_signal("button::press", State.get_client_callback())
end

local function disarm_click_to_close()
	local root_btns = State.get_root_buttons()
	if root_btns then
		pcall(function()
			root.buttons(root_btns)
		end)
	end
	State.clear_root_buttons()

	local client_cb = State.get_client_callback()
	if client_cb then
		pcall(function()
			client.disconnect_signal("button::press", client_cb)
		end)
	end
	State.clear_client_callback()
end

-- ---------------------------------------------------------------------------
-- Cleanup
local function ensure_closed()
	local menu = State.get_menu()

	if menu and menu.wibox and menu.wibox.valid then
		pcall(function()
			menu:hide()
		end)
	end

	State.clear_menu()

	Keygrab.stop()
	disarm_click_to_close()
end

-- ---------------------------------------------------------------------------
-- Anzeigen

function Menu.show_for_tabs_widget_with_clients_at(s, _widget, clients, anchor)
	local items = get_client_items(clients)
	ensure_closed()

	local menu = awful.menu({ items = items, theme = theme_block() })
	State.set_menu(menu)

	local x, y = Place.coords_for_tabs(s, anchor and anchor.x_left, #items)
	menu:show({ coords = { x = x, y = y } })

	if menu and menu.wibox then
		menu.wibox:connect_signal("unmanage", function()
			ensure_closed()
		end)
		menu.wibox:connect_signal("property::visible", function(wb)
			if not wb.visible then
				ensure_closed()
			end
		end)
	end

	arm_click_to_close()
	Keygrab.start(function()
		Menu.hide()
	end)
end

function Menu.show_for_start_widget(s, _widget)
	local items = Menu.get_start_items()
	ensure_closed()

	local menu = awful.menu({ items = items, theme = theme_block() })
	State.set_menu(menu)

	local x, y = Place.coords_for_start(s, #items)
	menu:show({ coords = { x = x, y = y } })

	if menu and menu.wibox then
		menu.wibox:connect_signal("unmanage", function()
			ensure_closed()
		end)
		menu.wibox:connect_signal("property::visible", function(wb)
			if not wb.visible then
				ensure_closed()
			end
		end)
	end

	arm_click_to_close()
	Keygrab.start(function()
		Menu.hide()
	end)
end

function Menu.build_main()
	return awful.menu({ items = Menu.get_start_items(), theme = theme_block() })
end

function Menu.hide()
	ensure_closed()
end

return Menu
