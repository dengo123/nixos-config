-- ~/.config/awesome/rules.lua
local awful = require("awful")
local beautiful = require("beautiful")

return {
	-- Standard
	{
		rule = {},
		properties = {
			border_width = beautiful.border_width,
			border_color = beautiful.border_normal,
			focus = awful.client.focus.filter,
			raise = true,
			screen = awful.screen.preferred,
			placement = awful.placement.no_overlap + awful.placement.no_offscreen,
		},
	},

	-- Floating typische Dialoge/Tools
	{
		rule_any = {
			class = { "Arandr", "Blueman-manager", "Gpick", "Pavucontrol" },
			role = { "pop-up" },
			type = { "dialog" },
		},
		properties = { floating = true },
	},

	-- Titlebars für normale/dialog Clients
	{
		rule_any = { type = { "normal", "dialog" } },
		properties = { titlebars_enabled = true },
	},
}
