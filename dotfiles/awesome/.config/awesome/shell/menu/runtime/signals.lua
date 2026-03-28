-- ~/.config/awesome/shell/menu/runtime/signals.lua
local awful = require("awful")

local M = {}

local runtime = {
	menu = nil,
	signals_ready = false,
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function menu()
	return runtime.menu
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(opts)
	opts = opts or {}
	runtime.menu = opts.menu or runtime.menu

	if runtime.signals_ready then
		return M
	end

	runtime.signals_ready = true

	awesome.connect_signal("menu::toggle", function()
		local Menu = menu()
		if not Menu then
			return
		end

		if Menu.is_open() then
			Menu.hide()
		else
			Menu.show_for_keybind(awful.screen.focused())
		end
	end)

	awesome.connect_signal("menu::open_start", function()
		local Menu = menu()
		if not Menu then
			return
		end

		if not Menu.is_open() then
			Menu.show_for_keybind(awful.screen.focused())
		end
	end)

	awesome.connect_signal("menu::close", function()
		local Menu = menu()
		if not Menu then
			return
		end

		if Menu.is_open() then
			Menu.hide()
		end
	end)

	return M
end

return M
