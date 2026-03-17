-- ~/.config/awesome/shell/menu/lib/popup.lua
local awful = require("awful")

local State = require("shell.menu.lib.state")

local M = {}

-- =========================================================================
-- Helpers
-- =========================================================================

local function attach_cleanup(menu, on_closed)
	if not (menu and menu.wibox) then
		return
	end

	menu.wibox:connect_signal("unmanage", function()
		if type(on_closed) == "function" then
			on_closed()
		end
	end)

	menu.wibox:connect_signal("property::visible", function(wibox)
		if wibox and not wibox.visible then
			if type(on_closed) == "function" then
				on_closed()
			end
		end
	end)
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.show(args)
	args = args or {}

	-- ---------------------------------------------------------------------
	-- Config
	-- ---------------------------------------------------------------------

	local items = assert(type(args.items) == "table" and #args.items > 0, "menu.popup: items fehlen/leer")
	local coords = assert(type(args.coords) == "table", "menu.popup: coords fehlen/ungueltig")
	local on_closed = args.on_closed

	-- ---------------------------------------------------------------------
	-- Build
	-- ---------------------------------------------------------------------

	local menu = awful.menu({
		items = items,
	})

	State.set_menu(menu)

	-- ---------------------------------------------------------------------
	-- Show
	-- ---------------------------------------------------------------------

	menu:show({
		coords = {
			x = coords.x,
			y = coords.y,
		},
	})

	attach_cleanup(menu, on_closed)

	return menu
end

function M.hide()
	local menu = State.get_menu()

	if menu and menu.wibox and menu.wibox.valid then
		pcall(function()
			menu:hide()
		end)
	end

	State.clear_menu()
end

return M
