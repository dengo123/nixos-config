-- ~/.config/awesome/shell/menu/init.lua
local awful = require("awful")
local beautiful = require("beautiful")
local hotkeys_popup = require("awful.hotkeys_popup")

local Menu = {}

-- Kontext (für Start-Items aus ui/cfg)
local _ctx = { ui = nil, cfg = nil }

-- Einfacher globaler Handle auf das aktuell offene Menü
local _state = { menu = nil }

-- ---------------------------------------------------------------------------

local function ensure_closed()
	if _state.menu and _state.menu.wibox and _state.menu.wibox.valid then
		_state.menu:hide()
	end
	_state.menu = nil
end

local function theme_min()
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

local function bar_geometry(s)
	local wb = s and (s.mywibar or s.mywibox or s.wibar)
	if wb and wb.valid then
		return wb:geometry()
	end
	return nil
end

local function screen_geometry(s)
	if s and s.geometry then
		return s.geometry
	end
	return { x = 0, y = 0, width = 0, height = 0 }
end

-- y-Position für „über der Bar“ (oder unter der Bar, falls Bar oben sitzt)
local function compute_y_for_popup(s, total_h)
	local pos = beautiful.wibar_position or "bottom"
	local bg = bar_geometry(s)
	if bg then
		if pos == "bottom" then
			return bg.y - total_h - 4
		else -- "top"
			return bg.y + bg.height + 4
		end
	end
	-- Fallback ohne Bar: oberhalb des unteren Bildschirmrands
	local sg = screen_geometry(s)
	return sg.y + math.max(0, sg.height - total_h - ((tonumber(beautiful.wibar_height) or 28) + 4))
end

-- ---------------------------------------------------------------------------
-- Öffentliche Initialisierung

function Menu.init(args)
	args = args or {}
	_ctx.ui = args.ui or {}
	_ctx.cfg = args.cfg or {}
end

-- ---------------------------------------------------------------------------
-- Start-Menü-Items (SSOT: Theme > cfg > Defaults)

function Menu.get_start_items()
	local ui, cfg = _ctx.ui, _ctx.cfg
	local theme_menu = ui and ui.theme and ui.theme.menu
	local items = (theme_menu and type(theme_menu.items) == "table" and theme_menu.items)
		or (cfg and cfg.menus and type(cfg.menus.items) == "table" and cfg.menus.items)
	if items then
		return items
	end

	local launcher_cmd = (cfg and type(cfg.launcher) == "string" and #cfg.launcher > 0) and cfg.launcher
		or "rofi -show drun"
	local files_cmd = (cfg and cfg.files_cmd and #cfg.files_cmd > 0) and cfg.files_cmd or "nemo"

	return {
		{
			"power",
			function()
				awesome.emit_signal("menu::power")
			end,
		},
		{
			"hotkeys",
			function()
				hotkeys_popup.show_help(nil, awful.screen.focused())
			end,
		},
		{
			"launcher",
			function()
				awful.spawn.with_shell(launcher_cmd)
			end,
		},
		{
			"files",
			function()
				awful.spawn.with_shell(files_cmd)
			end,
		},
	}
end

-- ---------------------------------------------------------------------------
-- Tabs: Clients → Items

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
-- FIXE Platzierung: linksbündig ÜBER dem Widget (unabhängig von der Maus)

-- Tabs: erwartet explizit die linke X-Kante (screen-absolute Koordinate)
function Menu.show_for_tabs_widget_with_clients_at(s, widget, clients, anchor)
	local items = build_client_items(clients)
	if not items or #items == 0 then
		return
	end

	ensure_closed()
	_state.menu = awful.menu({ items = items, theme = theme_min() })

	-- Höhe des Menüs = items * item-height
	local item_h = beautiful.menu_height or 24
	local total_h = item_h * #items

	local x_left = (anchor and anchor.x_left) or 0
	local y_top = compute_y_for_popup(s, total_h)

	-- Koordinaten-Show: kein „am Cursor“, sondern exakt an x_left/y_top
	_state.menu:show({ coords = { x = x_left, y = y_top } })
end

-- Start: kein Anchor übergeben → am linken Rand des Start-Widgets/Bar ausrichten
function Menu.show_for_start_widget(s, widget)
	local items = Menu.get_start_items()
	if not items or #items == 0 then
		return
	end

	ensure_closed()
	_state.menu = awful.menu({ items = items, theme = theme_min() })

	local item_h = beautiful.menu_height or 24
	local total_h = item_h * #items

	-- x-Position: bestmöglich am linken Bar-Rand ausrichten
	local x_left = 0
	do
		local bg = bar_geometry(s)
		if bg then
			x_left = bg.x + 8
		else
			local sg = screen_geometry(s)
			x_left = sg.x + 8
		end
	end

	local y_top = compute_y_for_popup(s, total_h)

	_state.menu:show({ coords = { x = x_left, y = y_top } })
end

-- ---------------------------------------------------------------------------
-- Optional: Kompatible Helfer

function Menu.build_main()
	return awful.menu({ items = Menu.get_start_items(), theme = theme_min() })
end

function Menu.hide()
	ensure_closed()
end

return Menu
