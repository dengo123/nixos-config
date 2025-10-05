-- ~/.config/awesome/shell/menu/init.lua
-- Eigenständiges Menü auf Basis von awful.menu
-- - Kein Backdrop, keine Struts/Docks
-- - Click-to-Close via temporären Root-Buttons + Client-Click-Signal
-- - ESC-to-Close (Keygrabber)
-- - Platzierung aus shell/menu/placement.lua

local awful = require("awful")
local beautiful = require("beautiful")
local gears = require("gears")

local Items = require("shell.menu.items")
local Place = require("shell.menu.placement")

local Menu = {}
local _ctx = { ui = nil, cfg = nil, dialogs = nil }
local _state = {
	menu = nil,
	keygrab = nil,
	root_btns = nil, -- gespeicherte Root-Buttons
	client_cb = nil, -- temporärer Client-Click-Handler
}

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
	args = args or {}
	_ctx.ui = args.ui or {}
	_ctx.cfg = args.cfg or {}
	_ctx.dialogs = args.dialogs
end

function Menu.get_start_items()
	assert(Items and type(Items.build_start) == "function", "shell.menu: items.build_start(ctx) fehlt")
	local items = Items.build_start(_ctx)
	assert(type(items) == "table" and #items > 0, "shell.menu: Start-Items leer")
	return items
end

local function get_client_items(clients)
	assert(Items and type(Items.build_clients) == "function", "shell.menu: items.build_clients(clients, ctx) fehlt")
	local items = Items.build_clients(clients, _ctx)
	assert(type(items) == "table" and #items > 0, "shell.menu: Tabs-Items leer")
	return items
end

-- ---------------------------------------------------------------------------
-- ESC-to-Close
local function start_keygrab()
	if _state.keygrab then
		return
	end
	_state.keygrab = awful.keygrabber.run(function(_, key, event)
		if event == "press" and key == "Escape" then
			Menu.hide()
			return false
		end
		return true
	end)
end

local function stop_keygrab()
	if _state.keygrab then
		pcall(function()
			awful.keygrabber.stop(_state.keygrab)
		end)
	end
	_state.keygrab = nil
end

-- Click-to-Close: temporäre Root-Buttons + Client-Klick
local function arm_click_to_close()
	-- Root: alte Buttons sichern, temporär erweitern
	_state.root_btns = root.buttons()
	local closer = function()
		Menu.hide()
	end
	local tmp = gears.table.join(
		_state.root_btns or {},
		awful.button({}, 1, closer),
		awful.button({}, 2, closer),
		awful.button({}, 3, closer)
	)
	root.buttons(tmp)

	-- Clients: jeder Klick irgendwo in einem Client schließt Menü
	_state.client_cb = function(_c, _x, _y, _button)
		Menu.hide()
	end
	client.connect_signal("button::press", _state.client_cb)
end

local function disarm_click_to_close()
	-- Root-Buttons wiederherstellen
	if _state.root_btns then
		pcall(function()
			root.buttons(_state.root_btns)
		end)
	end
	_state.root_btns = nil

	-- Client-Signal lösen
	if _state.client_cb then
		pcall(function()
			client.disconnect_signal("button::press", _state.client_cb)
		end)
	end
	_state.client_cb = nil
end

-- ---------------------------------------------------------------------------
-- Cleanup
local function ensure_closed()
	-- Menü schließen
	if _state.menu and _state.menu.wibox and _state.menu.wibox.valid then
		pcall(function()
			_state.menu:hide()
		end)
	end
	_state.menu = nil

	-- Grabber/Sniffer aus
	stop_keygrab()
	disarm_click_to_close()
end

-- ---------------------------------------------------------------------------
-- Anzeigen

-- Tabs-Popup: linksbündig über dem angeklickten Tab (Anchor wird von tabs.lua geliefert)
function Menu.show_for_tabs_widget_with_clients_at(s, _widget, clients, anchor)
	local items = get_client_items(clients)
	ensure_closed()

	_state.menu = awful.menu({ items = items, theme = theme_block() })

	-- Position aus Placement-Modul
	local x, y = Place.coords_for_tabs(s, anchor and anchor.x_left, #items)
	_state.menu:show({ coords = { x = x, y = y } })

	-- Auto-Cleanup wenn Menü „verschwindet“
	if _state.menu and _state.menu.wibox then
		_state.menu.wibox:connect_signal("unmanage", function()
			ensure_closed()
		end)
		_state.menu.wibox:connect_signal("property::visible", function(wb)
			if not wb.visible then
				ensure_closed()
			end
		end)
	end

	-- Click-to-Close + ESC-to-Close aktivieren
	arm_click_to_close()
	start_keygrab()
end

-- Start-Popup: linksbündig an der Bar
function Menu.show_for_start_widget(s, _widget)
	local items = Menu.get_start_items()
	ensure_closed()

	_state.menu = awful.menu({ items = items, theme = theme_block() })

	local x, y = Place.coords_for_start(s, #items)
	_state.menu:show({ coords = { x = x, y = y } })

	if _state.menu and _state.menu.wibox then
		_state.menu.wibox:connect_signal("unmanage", function()
			ensure_closed()
		end)
		_state.menu.wibox:connect_signal("property::visible", function(wb)
			if not wb.visible then
				ensure_closed()
			end
		end)
	end

	arm_click_to_close()
	start_keygrab()
end

-- Optional: klassisches Main-Menu (z. B. für Tests)
function Menu.build_main()
	return awful.menu({ items = Menu.get_start_items(), theme = theme_block() })
end

function Menu.hide()
	ensure_closed()
end

return Menu
