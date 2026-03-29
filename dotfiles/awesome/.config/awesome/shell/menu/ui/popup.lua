-- ~/.config/awesome/shell/menu/ui/popup.lua
local awful = require("awful")
local gears = require("gears")

local M = {}

local runtime = {
	menu = nil,
	mousegrabber_running = false,
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function current_menu()
	return runtime.menu
end

local function clear_menu()
	runtime.menu = nil
end

local function stop_close_guard()
	if runtime.mousegrabber_running and mousegrabber and type(mousegrabber.stop) == "function" then
		pcall(function()
			mousegrabber.stop()
		end)
	end

	runtime.mousegrabber_running = false
end

local function menu_geometry(menu)
	if not (menu and menu.wibox and menu.wibox.valid) then
		return nil
	end

	return menu.wibox:geometry()
end

local function point_in_rect(x, y, g)
	if not g then
		return false
	end

	return x >= g.x and x < (g.x + g.width) and y >= g.y and y < (g.y + g.height)
end

local function is_any_mouse_button_pressed(mouse_state)
	local buttons = mouse_state and mouse_state.buttons or {}
	return buttons[1] or buttons[2] or buttons[3]
end

local function start_close_guard()
	stop_close_guard()

	if not (mousegrabber and type(mousegrabber.run) == "function") then
		return
	end

	local pressed_before = true
	runtime.mousegrabber_running = true

	mousegrabber.run(function(mouse_state)
		local menu = current_menu()

		if not (menu and menu.wibox and menu.wibox.valid and menu.wibox.visible) then
			runtime.mousegrabber_running = false
			return false
		end

		local pressed_now = is_any_mouse_button_pressed(mouse_state)

		if pressed_now and not pressed_before then
			local g = menu_geometry(menu)
			local inside = point_in_rect(mouse_state.x, mouse_state.y, g)

			if not inside then
				M.hide()
				runtime.mousegrabber_running = false
				return false
			end
		end

		pressed_before = pressed_now
		return true
	end, "left_ptr")
end

local function attach_cleanup(menu, on_closed)
	if not (menu and menu.wibox) then
		return
	end

	menu.wibox:connect_signal("unmanage", function()
		if current_menu() == menu then
			clear_menu()
		end

		stop_close_guard()

		if type(on_closed) == "function" then
			on_closed()
		end
	end)

	menu.wibox:connect_signal("property::visible", function(wb)
		if wb and not wb.visible then
			if current_menu() == menu then
				clear_menu()
			end

			stop_close_guard()

			if type(on_closed) == "function" then
				on_closed()
			end
		end
	end)
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.show(opts)
	opts = opts or {}

	local items = opts.items
	assert(type(items) == "table" and #items > 0, "menu.ui.popup: items fehlen/leer")

	local theme = opts.theme
	assert(type(theme) == "table", "menu.ui.popup: theme fehlt/ungueltig")

	local coords = opts.coords
	assert(type(coords) == "table", "menu.ui.popup: coords fehlen/ungueltig")

	local on_closed = opts.on_closed

	M.hide()

	local menu = awful.menu({
		items = items,
		theme = theme,
	})

	runtime.menu = menu

	menu:show({
		coords = {
			x = coords.x,
			y = coords.y,
		},
	})

	attach_cleanup(menu, on_closed)

	gears.timer.delayed_call(function()
		if current_menu() == menu and menu.wibox and menu.wibox.valid and menu.wibox.visible then
			start_close_guard()
		end
	end)

	return menu
end

function M.is_open()
	local menu = current_menu()

	return menu ~= nil and menu.wibox ~= nil and menu.wibox.valid and menu.wibox.visible == true
end

function M.hide()
	local menu = current_menu()

	stop_close_guard()

	if menu and menu.wibox and menu.wibox.valid then
		pcall(function()
			menu:hide()
		end)
	end

	clear_menu()
end

return M
