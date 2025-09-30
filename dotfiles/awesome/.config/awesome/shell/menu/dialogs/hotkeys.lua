-- ~/.config/awesome/features/shell/menu/dialogs/hotkeys/init.lua
local gears = require("gears")
local beautiful = require("beautiful")
beautiful.hotkeys_bg = "#ECE9D8"
beautiful.hotkeys_fg = "#000000"
beautiful.hotkeys_modifiers_fg = "#000000"
beautiful.hotkeys_border_width = 4
beautiful.hotkeys_border_color = "#235CDB"
beautiful.hotkeys_font = "Inter 10"
beautiful.hotkeys_description_font = "Inter 10"
beautiful.hotkeys_group_margin = 16
beautiful.hotkeys_shape = gears.shape.rounded_rect

-- Standard-Gruppen registrieren
pcall(require, "awful.hotkeys_popup.keys")

local M = {}

-- Öffentliche API: kompatibel zu deiner Registry
function M.hotkeys()
	local awful, hk, gears = require("awful"), require("awful.hotkeys_popup"), require("gears")

	local api = rawget(_G, "__menu_api")
	local restore
	if api and type(api.set_ontop) == "function" then
		-- ontop des Menüs temporär aus
		restore = api.set_ontop(false)
	elseif api and type(api.lower) == "function" then
		api.lower()
	end

	gears.timer.delayed_call(function()
		hk.show_help(nil, awful.screen.focused())
	end)

	return {
		close = function()
			pcall(hk.hide_help)
			if type(restore) == "function" then
				restore()
			end
		end,
	}
end

return M
