-- ~/.config/awesome/shell/menu/ui/popup.lua
local awful = require("awful")
local gears = require("gears")

local M = {}

local runtime = {
	menu = nil,
	root_buttons = nil,
	client_callback = nil,
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

local function disarm_close_guard()
	if runtime.root_buttons then
		pcall(function()
			root.buttons(runtime.root_buttons)
		end)
	end

	runtime.root_buttons = nil

	if runtime.client_callback then
		pcall(function()
			client.disconnect_signal("button::press", runtime.client_callback)
		end)
	end

	runtime.client_callback = nil
end

local function attach_cleanup(menu, on_closed)
	if not (menu and menu.wibox) then
		return
	end

	menu.wibox:connect_signal("unmanage", function()
		if current_menu() == menu then
			clear_menu()
		end

		disarm_close_guard()

		if type(on_closed) == "function" then
			on_closed()
		end
	end)

	menu.wibox:connect_signal("property::visible", function(wb)
		if wb and not wb.visible then
			if current_menu() == menu then
				clear_menu()
			end

			disarm_close_guard()

			if type(on_closed) == "function" then
				on_closed()
			end
		end
	end)
end

local function arm_close_guard(on_close)
	local function closer()
		if type(on_close) == "function" then
			on_close()
		end
	end

	runtime.root_buttons = root.buttons()

	local tmp = gears.table.join(
		runtime.root_buttons or {},
		awful.button({}, 1, closer),
		awful.button({}, 2, closer),
		awful.button({}, 3, closer)
	)

	root.buttons(tmp)

	local client_cb = function()
		closer()
	end

	runtime.client_callback = client_cb
	client.connect_signal("button::press", client_cb)
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
	arm_close_guard(function()
		M.hide()
	end)

	return menu
end

function M.is_open()
	local menu = current_menu()

	return menu ~= nil and menu.wibox ~= nil and menu.wibox.valid and menu.wibox.visible == true
end

function M.hide()
	local menu = current_menu()

	disarm_close_guard()

	if menu and menu.wibox and menu.wibox.valid then
		pcall(function()
			menu:hide()
		end)
	end

	clear_menu()
end

return M
