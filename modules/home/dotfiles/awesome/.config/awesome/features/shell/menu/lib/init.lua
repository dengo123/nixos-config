-- ~/.config/awesome/features/shell/menu/lib/apps.lua
local awful = require("awful")
local beautiful = require("beautiful")

-- Hotkeys-Popup (Standard)
local hotkeys_popup = require("awful.hotkeys_popup")
pcall(require, "awful.hotkeys_popup.keys") -- füllt die Gruppen (awesome/client/layout/…)

local D = {}

-- User-Block
D.user = {
	name = os.getenv("USER") or "user",
	avatar = beautiful.awesome_icon,
}

-- Linke Spalte (statisch)
D.left_items = {
	{ text = "All Programs", icon = beautiful.awesome_icon, on_press = function() end },
	{
		text = "Internet",
		icon = beautiful.awesome_icon,
		on_press = function()
			awful.spawn("firefox")
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

-- Rechte Spalte (XP-Feeling)
D.right_items = {
	{
		text = "My Desktop",
		icon = beautiful.awesome_icon,
		on_press = function()
			awful.spawn.with_shell('xdg-open "${XDG_DESKTOP_DIR:-$HOME/Desktop}"')
		end,
	},
	{
		text = "My Documents", -- zeigt $HOME
		icon = beautiful.awesome_icon,
		on_press = function()
			awful.spawn.with_shell('xdg-open "$HOME"')
		end,
	},
	{
		text = "Documents",
		icon = beautiful.awesome_icon,
		on_press = function()
			awful.spawn.with_shell('xdg-open "${XDG_DOCUMENTS_DIR:-$HOME/Documents}"')
		end,
	},
	{
		text = "Downloads",
		icon = beautiful.awesome_icon,
		on_press = function()
			awful.spawn.with_shell('xdg-open "${XDG_DOWNLOAD_DIR:-$HOME/Downloads}"')
		end,
	},
	{
		text = "My Computer", -- dein nixos-config Repo
		icon = beautiful.awesome_icon,
		on_press = function()
			awful.spawn.with_shell('xdg-open "$HOME/nixos-config"')
		end,
	},
	{
		text = "Help and Support",
		icon = beautiful.awesome_icon,
		on_press = function()
			hotkeys_popup.show_help(nil, awful.screen.focused())
			-- Optional: Menü danach schließen
			local api = rawget(_G, "__menu_api")
			if api and api.hide then
				api.hide()
			end
		end,
	},
}

-- Power-Leiste
D.power_items = {
	{
		text = "Log Off",
		icon = beautiful.awesome_icon,
		on_press = function()
			awesome.quit()
		end,
	},
	{
		text = "Turn Off",
		icon = beautiful.awesome_icon,
		on_press = function()
			awful.spawn.with_shell("systemctl poweroff")
		end,
	},
}

return D
