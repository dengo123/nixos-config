-- ~/.config/awesome/shell/menu/init.lua
local awful = require("awful")
local beautiful = require("beautiful")

-- Direkt die Root-Module laden (keine lib/init.lua mehr)
local Items = require("shell.menu.items")
local Place = require("shell.menu.placement")

local Menu = {}
local _ctx = { ui = nil, cfg = nil }
local _state = { menu = nil }

-- ---------------------------------------------------------------------------

local function ensure_closed()
	if _state.menu and _state.menu.wibox and _state.menu.wibox.valid then
		_state.menu:hide()
	end
	_state.menu = nil
end

local function theme_block()
	-- keine Fallbacks: diese Keys MÜSSEN vom Theme gesetzt sein
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

function Menu.init(args)
	args = args or {}
	_ctx.ui = args.ui or {}
	_ctx.cfg = args.cfg or {}
	_ctx.dialogs = args.dialogs
	-- kein Aggregator/Lib.attach mehr nötig
end

-- Start-Items direkt aus Items-Modul
function Menu.get_start_items()
	assert(Items and type(Items.build_start) == "function", "shell.menu: items.build_start(ctx) fehlt")
	local items = Items.build_start(_ctx)
	assert(type(items) == "table" and #items > 0, "shell.menu: Start-Items leer")
	return items
end

-- Tabs: Clients -> Items (strict)
local function get_client_items(clients)
	assert(Items and type(Items.build_clients) == "function", "shell.menu: items.build_clients(clients, ctx) fehlt")
	local items = Items.build_clients(clients, _ctx)
	assert(type(items) == "table" and #items > 0, "shell.menu: Tabs-Items leer")
	return items
end

-- ---------------------------------------------------------------------------

-- Tabs-Popup: linksbündig über dem angeklickten Tab (Anchor wird von tabs.lua geliefert)
function Menu.show_for_tabs_widget_with_clients_at(s, _widget, clients, anchor)
	local items = get_client_items(clients)
	assert(Place, "shell.menu: placement-API fehlt")

	ensure_closed()
	_state.menu = awful.menu({ items = items, theme = theme_block() })

	local item_h = assert(tonumber(beautiful.menu_height), "theme: beautiful.menu_height ungültig")
	local total_h = item_h * #items

	local x = Place.x_left_from_anchor(s, anchor and anchor.x_left)
	local y = Place.y_over_bar(s, total_h)

	_state.menu:show({ coords = { x = x, y = y } })
end

-- Start-Popup: linksbündig an der Bar
function Menu.show_for_start_widget(s, _widget)
	local items = Menu.get_start_items()
	assert(Place, "shell.menu: placement-API fehlt")

	ensure_closed()
	_state.menu = awful.menu({ items = items, theme = theme_block() })

	local item_h = assert(tonumber(beautiful.menu_height), "theme: beautiful.menu_height ungültig")
	local total_h = item_h * #items

	local x = Place.x_left_on_bar(s)
	local y = Place.y_over_bar(s, total_h)

	_state.menu:show({ coords = { x = x, y = y } })
end

-- Optional: klassisches Main-Menu (z. B. für Tests)
function Menu.build_main()
	return awful.menu({ items = Menu.get_start_items(), theme = theme_block() })
end

function Menu.hide()
	ensure_closed()
end

return Menu
