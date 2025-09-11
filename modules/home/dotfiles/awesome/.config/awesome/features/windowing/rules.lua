-- ~/.config/awesome/features/windowing/rules.lua
local awful = require("awful")
local beautiful = require("beautiful")

local M = {}

function M.apply(o)
	local modkey = o.modkey
	local mouse = o.mouse

	awful.rules.rules = {
		{
			rule = {},
			properties = {
				border_width = beautiful.border_width,
				border_color = beautiful.border_normal,
				focus = awful.client.focus.filter,
				raise = true,
				-- keine clientkeys mehr (promote-to-master ist global)
				buttons = mouse and mouse.client_buttons and mouse.client_buttons(modkey) or nil,
				screen = awful.screen.preferred,
				placement = awful.placement.no_overlap + awful.placement.no_offscreen,
			},
		},
		{
			rule_any = {
				instance = { "DTA", "copyq", "pinentry" },
				class = {
					"Arandr",
					"Blueman-manager",
					"Gpick",
					"Kruler",
					"MessageWin",
					"Sxiv",
					"Tor Browser",
					"Wpa_gui",
					"veromix",
					"xtightvncviewer",
				},
				name = { "Event Tester" },
				role = { "AlarmWindow", "ConfigManager", "pop-up" },
			},
			properties = { floating = true },
		},
		{
			rule_any = { type = { "normal", "dialog" } },
			properties = { titlebars_enabled = true },
		},
	}
end

return M
