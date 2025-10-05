-- ~/.config/awesome/shell/menu/init.lua
-- Eigenständiges Menü auf Basis von awful.menu
-- - Keine Backdrops, keine Struts/Docks
-- - Click-to-Close (Klick außerhalb des Menüs)
-- - ESC-to-Close (Keygrabber)
-- - Platzierung komplett aus shell/menu/placement.lua

local awful = require("awful")
local beautiful = require("beautiful")

local Items = require("shell.menu.items")
local Place = require("shell.menu.placement")

local Menu = {}
local _ctx = { ui = nil, cfg = nil, dialogs = nil }
local _state = { menu = nil }

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
-- Click-to-Close & ESC-to-Close
local _mousegrabber_running = false
local _keygrabber_token = nil

local function stop_mousegrabber()
	if _mousegrabber_running then
		pcall(function()
			mousegrabber.stop()
		end) -- <-- global mousegrabber
		_mousegrabber_running = false
	end
end

local function stop_keygrabber()
	if _keygrabber_token ~= nil then
		pcall(function()
			awful.keygrabber.stop(_keygrabber_token)
		end)
		_keygrabber_token = nil
	end
end

local function point_in_rect(mx, my, r)
	return mx >= r.x and mx < (r.x + r.width) and my >= r.y and my < (r.y + r.height)
end

local function start_click_to_close()
	if not (_state.menu and _state.menu.wibox and _state.menu.wibox.valid) then
		return
	end
	local wb = _state.menu.wibox
	_mousegrabber_running = true
	mousegrabber.run(function(m) -- <-- global mousegrabber
		-- Schließen bei beliebigem Klick außerhalb der Menü-Wibox
		if m.buttons[1] or m.buttons[2] or m.buttons[3] or m.buttons[4] or m.buttons[5] then
			local g = wb:geometry()
			if not point_in_rect(m.x, m.y, g) then
				Menu.hide()
				return false
			end
		end
		return true
	end, "left_ptr")
end

local function start_esc_to_close()
	_keygrabber_token = awful.keygrabber.run(function(_, key, event)
		if event == "release" then
			return
		end
		if key == "Escape" then
			Menu.hide()
			return false
		end
		return true
	end)
end

-- ---------------------------------------------------------------------------
-- Cleanup
local function ensure_closed()
	-- Menü zu
	if _state.menu and _state.menu.wibox and _state.menu.wibox.valid then
		pcall(function()
			_state.menu:hide()
		end)
	end
	_state.menu = nil

	-- Grabber stoppen
	stop_mousegrabber()
	stop_keygrabber()
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
	start_click_to_close()
	start_esc_to_close()
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

	start_click_to_close()
	start_esc_to_close()
end

-- Optional: klassisches Main-Menu (z. B. für Tests)
function Menu.build_main()
	return awful.menu({ items = Menu.get_start_items(), theme = theme_block() })
end

function Menu.hide()
	ensure_closed()
end

return Menu
