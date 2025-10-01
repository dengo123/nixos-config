-- ~/.config/awesome/shell/windowing/rules.lua
local awful = require("awful")
local beautiful = require("beautiful")

local M = {}

-- kleine Whitelist für GUI-Filemanager
local FILEMANAGER_CLASSES = {
	"Nemo",
	"Thunar",
	"Dolphin",
	"org.gnome.Nautilus",
	"Nautilus",
	"Pcmanfm",
	"Pcmanfm-qt",
	"Caja",
	"Spacefm",
	"Krusader",
	"Double Commander",
}

function M.apply(o)
	local modkey = o.modkey
	local mouse = o.mouse

	awful.rules.rules = {
		-- Default
		{
			rule = {},
			properties = {
				border_width = beautiful.border_width,
				border_color = beautiful.border_normal,
				focus = awful.client.focus.filter,
				raise = true,
				buttons = mouse and mouse.client_buttons and mouse.client_buttons(modkey) or nil,
				screen = awful.screen.preferred,
				placement = awful.placement.no_overlap + awful.placement.no_offscreen,
			},
		},

		-- GUI-Filemanager immer floating
		{
			rule_any = { class = FILEMANAGER_CLASSES },
			properties = {
				floating = true,
				placement = awful.placement.centered,
			},
		},

		-- „Werkzeug“-Fenster & Popups generell floating (deckt sehr viel ab)
		{
			rule_any = {
				type = { "dialog", "utility", "toolbar", "splash" },
				role = { "pop-up", "Preferences" },
			},
			properties = {
				floating = true,
				placement = awful.placement.centered,
			},
		},

		-- Blueman Manager zuverlässig erwischen (class *oder* instance)
		{
			rule_any = {
				class = { "Blueman-manager", "blueman-manager", "Blueman" },
				instance = { "blueman-manager" },
				name = { "Bluetooth", "Blueman" }, -- Falls class/instance abweicht
			},
			properties = {
				floating = true,
				placement = awful.placement.centered,
			},
		},

		-- Sonstige Utilities (deine Liste)
		{
			rule_any = {
				instance = { "DTA", "copyq", "pinentry" },
				class = {
					"Arandr",
					"Gpick",
					"Kruler",
					"MessageWin",
					"Sxiv",
					"Tor Browser",
					"Wpa_gui",
					"veromix",
					"xtightvncviewer",
					-- "Blueman-manager", -- bereits oben abgedeckt
				},
				name = { "Event Tester" },
			},
			properties = { floating = true },
		},

		-- Titlebars für „normal“ & „dialog“
		{
			rule_any = { type = { "normal", "dialog" } },
			properties = { titlebars_enabled = true },
		},
	}
end

return M
