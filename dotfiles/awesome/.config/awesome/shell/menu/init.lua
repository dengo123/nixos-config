-- ~/.config/awesome/shell/menu/init.lua
local awful = require("awful")
local beautiful = require("beautiful")

local Lib = require("shell.menu.lib") -- zentrale Aggregation (u.a. placement)
local Dialogs = require("shell.menu.dialogs") -- Dialog-API

local Menu = {}

-- Kontext inkl. Dialog-API
local _ctx = { ui = nil, cfg = nil, dialogs = nil }
local _state = { menu = nil } -- offenes Menü-Handle

-- ---------------------------------------------------------------------------

local function ensure_closed()
	if _state.menu and _state.menu.wibox and _state.menu.wibox.valid then
		_state.menu:hide()
	end
	_state.menu = nil
end

local function theme_block()
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

-- Start-Items ausschließlich aus shell.menu.lib bereitstellen
local function require_start_items_from_lib()
	local items = (type(Lib.defaults) == "function") and Lib.defaults(_ctx) or nil
	assert(
		type(items) == "table" and #items > 0,
		"shell.menu: keine Start-Items gefunden (erwarte shell/menu/lib/items.lua)"
	)
	return items
end

-- Tabs: Clients → Items (nur Tasks der Gruppe)
local function build_client_items(clients)
	local items = {}
	for _, c in ipairs(clients or {}) do
		local label = c.name or c.class or "App"
		table.insert(items, {
			label,
			function()
				if c.valid then
					c:emit_signal("request::activate", "group_menu", { raise = true })
				end
			end,
			c.icon,
		})
	end
	return items
end

-- ---------------------------------------------------------------------------

function Menu.init(args)
	args = args or {}
	_ctx.ui = args.ui or {}
	_ctx.cfg = args.cfg or {}
	_ctx.dialogs = Dialogs.init({ ui = _ctx.ui, cfg = _ctx.cfg })

	-- Lib-Module an Menu anhängen (u.a. menu.lib.placement verfügbar machen)
	Lib.attach(Menu, {}) -- kein Flatten nötig; Zugriff über Menu.lib.*

	-- Optional exponieren (falls extern benötigt)
	Menu.dialogs = _ctx.dialogs
end

-- Exponiert für andere Module (z. B. Start-Button)
function Menu.get_start_items()
	return require_start_items_from_lib()
end

-- Tabs: Widget + Clients + expliziter Anchor (x_left in Screen-Koords)
function Menu.show_for_tabs_widget_with_clients_at(s, _widget, clients, anchor)
	local items = build_client_items(clients)
	if not items or #items == 0 then
		return
	end

	-- Placement aus der Lib beziehen
	local Place = Menu.lib and Menu.lib.placement
	assert(
		Place and Place.x_left_from_anchor and Place.y_over_bar,
		"shell.menu: placement-API fehlt (erwarte shell.menu.lib.placement)"
	)

	ensure_closed()
	_state.menu = awful.menu({ items = items, theme = theme_block() })

	local item_h = beautiful.menu_height or 24
	local total_h = item_h * #items

	local x = Place.x_left_from_anchor(s, anchor and anchor.x_left)
	local y = Place.y_over_bar(s, total_h)

	_state.menu:show({ coords = { x = x, y = y } })
end

-- Start: über Start-Button/Bar; Items aus Lib
function Menu.show_for_start_widget(s, _widget)
	local items = require_start_items_from_lib()

	-- Placement aus der Lib beziehen
	local Place = Menu.lib and Menu.lib.placement
	assert(
		Place and Place.x_left_on_bar and Place.y_over_bar,
		"shell.menu: placement-API fehlt (erwarte shell.menu.lib.placement)"
	)

	ensure_closed()
	_state.menu = awful.menu({ items = items, theme = theme_block() })

	local item_h = beautiful.menu_height or 24
	local total_h = item_h * #items

	local x = Place.x_left_on_bar(s)
	local y = Place.y_over_bar(s, total_h)

	_state.menu:show({ coords = { x = x, y = y } })
end

-- Optional-Compat
function Menu.build_main()
	return awful.menu({ items = require_start_items_from_lib(), theme = theme_block() })
end

function Menu.hide()
	ensure_closed()
end

return Menu
