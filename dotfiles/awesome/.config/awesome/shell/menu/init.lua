-- ~/.config/awesome/shell/menu/init.lua
local awful = require("awful")
local beautiful = require("beautiful")
local Lib = require("shell.menu.lib")
local Dialogs = require("shell.menu.dialogs")

local Menu = {}
local _ctx = { ui = nil, cfg = nil, dialogs = nil }
local _state = { menu = nil }

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

function Menu.init(args)
	args = args or {}
	_ctx.ui = args.ui or {}
	_ctx.cfg = args.cfg or {}
	_ctx.dialogs = Dialogs.init({ ui = _ctx.ui, cfg = _ctx.cfg })

	Lib.attach(Menu, {}) -- stellt Menu.lib.* bereit (inkl. items, placement)
	Menu.dialogs = _ctx.dialogs
end

function Menu.get_start_items()
	local items_mod = Menu.lib and Menu.lib.items
	assert(items_mod and type(items_mod.build_start) == "function", "shell.menu: lib.items.build_start(ctx) fehlt")
	local items = items_mod.build_start(_ctx)
	assert(type(items) == "table" and #items > 0, "shell.menu: Start-Items leer")
	return items
end

-- Tabs Popup (Clients -> Items)
local function build_client_items(clients)
	local items_mod = Menu.lib and Menu.lib.items
	if items_mod and type(items_mod.build_clients) == "function" then
		return items_mod.build_clients(clients, _ctx)
	end
	-- minimaler Fallback, falls build_clients nicht vorhanden ist
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

function Menu.show_for_tabs_widget_with_clients_at(s, _widget, clients, anchor)
	local items = build_client_items(clients)
	if not items or #items == 0 then
		return
	end

	local Place = Menu.lib and Menu.lib.placement
	assert(Place, "shell.menu: placement-API fehlt")

	ensure_closed()
	_state.menu = awful.menu({ items = items, theme = theme_block() })

	local item_h = beautiful.menu_height or 24
	local total_h = item_h * #items
	local x = Place.x_left_from_anchor(s, anchor and anchor.x_left)
	local y = Place.y_over_bar(s, total_h)
	_state.menu:show({ coords = { x = x, y = y } })
end

function Menu.show_for_start_widget(s, _widget)
	local items = Menu.get_start_items()

	local Place = Menu.lib and Menu.lib.placement
	assert(Place, "shell.menu: placement-API fehlt")

	ensure_closed()
	_state.menu = awful.menu({ items = items, theme = theme_block() })

	local item_h = beautiful.menu_height or 24
	local total_h = item_h * #items
	local x = Place.x_left_on_bar(s)
	local y = Place.y_over_bar(s, total_h)
	_state.menu:show({ coords = { x = x, y = y } })
end

function Menu.build_main()
	return awful.menu({ items = Menu.get_start_items(), theme = theme_block() })
end

function Menu.hide()
	ensure_closed()
end

return Menu
