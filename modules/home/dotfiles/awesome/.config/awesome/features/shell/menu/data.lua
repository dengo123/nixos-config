-- ~/.config/awesome/features/shell/menu/data.lua
local awful = require("awful")
local beautiful = require("beautiful")

local D = {}

D.user = {
	name = os.getenv("USER") or "user",
	avatar = beautiful.awesome_icon, -- später durch Pfad ersetzen
	subtitle = "dengo123",
}

D.left_items = {
	{ text = "All Programs", icon = beautiful.awesome_icon, on_press = function() end },
	{
		text = "Internet",
		icon = beautiful.awesome_icon,
		on_press = function()
			awful.spawn("zen-browser")
		end,
	},
	{
		text = "E-mail",
		icon = beautiful.awesome_icon,
		on_press = function()
			awful.spawn("thunderbird")
		end,
	},
}

D.right_items = {
	{
		text = "My Documents",
		icon = beautiful.awesome_icon,
		on_press = function()
			awful.spawn("xdg-open $HOME")
		end,
	},
	{
		text = "Control Panel",
		icon = beautiful.awesome_icon,
		on_press = function()
			awful.spawn("gnome-control-center")
		end,
	},
	{
		text = "Help and Support",
		icon = beautiful.awesome_icon,
		on_press = function()
			awful.spawn("yelp")
		end,
	},
}

D.power_items = {
	{
		text = "Log Off",
		icon = beautiful.awesome_icon,
		on_press = function()
			awesome.quit()
		end,
	},
	{
		text = "Turn Off Computer",
		icon = beautiful.awesome_icon,
		on_press = function()
			awful.spawn.with_shell("systemctl poweroff")
		end,
	},
}

return D
