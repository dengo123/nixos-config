-- ~/.config/awesome/features/shell/menu/lib/init.lua
local awful = require("awful")
local beautiful = require("beautiful")

-- Falls dein Hotkeys-Dialog diese Daten nutzt:
pcall(require, "awful.hotkeys_popup.keys")

local D = {}

-- -------------------------------------------------------------------
-- User-Block (Header)
-- -------------------------------------------------------------------
D.user = {
	name = os.getenv("USER") or "user",
	avatar = beautiful.awesome_icon,
	sub = nil, -- optional
}

-- -------------------------------------------------------------------
-- Linke Spalte (Schnellzugriffe)
-- -------------------------------------------------------------------
D.left_items = {
	{
		text = "All Programs",
		icon = beautiful.awesome_icon,
		on_press = function() end, -- Platzhalter
	},
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

-- -------------------------------------------------------------------
-- Rechte Spalte (Orte / Hilfe)
-- -------------------------------------------------------------------
D.right_items = {
	{
		text = "My Desktop",
		icon = beautiful.awesome_icon,
		on_press = function()
			awful.spawn.with_shell('xdg-open "${XDG_DESKTOP_DIR:-$HOME/Desktop}"')
		end,
	},
	{
		text = "My Documents",
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
		text = "My Computer",
		icon = beautiful.awesome_icon,
		on_press = function()
			awful.spawn.with_shell('xdg-open "$HOME/nixos-config"')
		end,
	},

	{
		text = "Control Panel",
		icon = beautiful.awesome_icon,
		dialog = "control_panel", -- wird über dialogs/init.lua aufgerufen
		dialog_args = {
			title = "Control Panel",
		},
	},
	-- Hilfe/Support öffnet Hotkeys-Dialog
	{
		text = "Help and Support",
		icon = beautiful.awesome_icon,
		dialog = "hotkeys", -- -> Dialogs.hotkeys()
		dialog_args = {
			title = "Keyboard Shortcuts",
		},
	},
}

-- -------------------------------------------------------------------
-- Power-Leiste (Footer rechts) – öffnet Dialoge
-- -------------------------------------------------------------------
D.power_items = {
	{
		text = "Log Off",
		icon = beautiful.awesome_icon,
		dialog = "logout", -- -> Dialogs.logout()
		-- dialog_args = { ... } -- optional
	},
	{
		text = "Turn Off",
		icon = beautiful.awesome_icon,
		dialog = "power", -- -> Dialogs.power()
		-- dialog_args = { ... } -- optional
	},
}

return D
