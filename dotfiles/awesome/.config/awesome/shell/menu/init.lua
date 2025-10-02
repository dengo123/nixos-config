-- ~/.config/awesome/shell/menu/init.lua
local awful = require("awful")
local wibox = require("wibox")
local beautiful = require("beautiful")
local hotkeys_popup = require("awful.hotkeys_popup")

local Menu = {}

local _state = {
	menu = nil,
	overlay = nil,
}

local function ensure_closed()
	if _state.menu and _state.menu.wibox and _state.menu.wibox.valid then
		_state.menu:hide()
	end
	if _state.overlay and _state.overlay.valid then
		_state.overlay.visible = false
		_state.overlay = nil
	end
	_state.menu = nil
end

-- Vollbild-Overlay je Screen: Klick irgendwo = Menü schließen
local function make_overlay(s)
	local g = s.geometry
	local ov = wibox({
		screen = s,
		x = g.x,
		y = g.y,
		width = g.width,
		height = g.height,
		ontop = true,
		visible = true,
		type = "dock",
		bg = "#00000000",
	})
	ov:connect_signal("button::press", ensure_closed)
	return ov
end

-- rechtsbündig an der Wibar; y abhängig von Bar-Position
local function coords_right_of_wibar(s, items)
	local wb = s.mywibox or s.mywibar or s.wibar
	assert(wb and wb.valid, "shell.menu: kein wibar/wibox am Screen (s.mywibox/s.mywibar)")
	local g = wb:geometry()
	local mw = assert(tonumber(beautiful.menu_width), "shell.menu: beautiful.menu_width fehlt/ungültig")
	local mh = assert(tonumber(beautiful.menu_height), "shell.menu: beautiful.menu_height fehlt/ungültig")
	local total_h = #items * mh
	local x = g.x + g.width - mw
	local pos = beautiful.wibar_position or "bottom"
	local y = (pos == "bottom") and (g.y - total_h) or (g.y + g.height)
	return x, y
end

-- zentrales Show (rechtsbündig an der Bar + Overlay)
local function show_at_bar_right(s, items)
	ensure_closed()
	_state.menu = awful.menu({
		items = items,
		theme = {
			width = beautiful.menu_width,
			height = beautiful.menu_height, -- item height
			bg_normal = beautiful.menu_bg_normal,
			fg_normal = beautiful.menu_fg_normal,
			bg_focus = beautiful.menu_bg_focus,
			fg_focus = beautiful.menu_fg_focus,
			border_color = beautiful.menu_border_color,
			border_width = beautiful.menu_border_width,
			shape = beautiful.menu_shape,
			submenu = beautiful.menu_submenu,
		},
	})
	local x, y = coords_right_of_wibar(s, items)
	_state.menu:show({ coords = { x = x, y = y } })
	_state.overlay = make_overlay(s)
end

-- ===== Öffentliche APIs =====================================================

-- Hauptmenü bauen (Items kommen aus ui.theme.menu.items oder cfg.menus.items;
-- wenn beides fehlt, werden exakt die geforderten Defaults verwendet)
function Menu.build_main(opts)
	opts = opts or {}
	local ui = opts.ui or {}
	local cfg = opts.cfg or {}

	local theme_menu = ui.theme and ui.theme.menu
	local items = (theme_menu and type(theme_menu.items) == "table" and theme_menu.items)
		or (cfg.menus and type(cfg.menus.items) == "table" and cfg.menus.items)

	if not items then
		-- *** geforderte Defaults ***
		local launcher_cmd = (type(cfg.launcher) == "string" and #cfg.launcher > 0) and cfg.launcher
			or "rofi -show drun"
		local files_cmd = (cfg.files_cmd and #cfg.files_cmd > 0) and cfg.files_cmd or "nemo"

		items = {
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

	return awful.menu({
		items = items,
		theme = {
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
		},
	})
end

-- Rechtsklick aus Tabs: Items = Taskliste der Gruppe
function Menu.show_tabs_group(s, items)
	show_at_bar_right(s, items)
end

-- Rechtsklick aus Start: Items = Start-spezifisches Menü (vom Theme/Caller)
function Menu.show_start(s, items)
	show_at_bar_right(s, items)
end

function Menu.hide()
	ensure_closed()
end

return Menu
