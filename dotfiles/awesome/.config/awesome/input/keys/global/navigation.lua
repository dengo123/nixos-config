-- ~/.config/awesome/input/keys/global/navigation.lua
local awful = require("awful")
local H = require("input.keys.helpers")

return function(modkey)
	return awful.util.table.join(
		-- Fokus bewegen
		awful.key({ modkey }, "Left", function()
			awful.client.focus.bydirection("left")
		end, { description = "focus window left", group = "client" }),
		awful.key({ modkey }, "Right", function()
			awful.client.focus.bydirection("right")
		end, { description = "focus window right", group = "client" }),
		awful.key({ modkey }, "Up", function()
			awful.client.focus.bydirection("up")
		end, { description = "focus window up", group = "client" }),
		awful.key({ modkey }, "Down", function()
			awful.client.focus.bydirection("down")
		end, { description = "focus window down", group = "client" }),

		-- Fenster bewegen
		awful.key({ modkey, "Shift" }, "Left", function()
			H.move_client_dir("left")
		end, { description = "move window left", group = "client" }),
		awful.key({ modkey, "Shift" }, "Right", function()
			H.move_client_dir("right")
		end, { description = "move window right", group = "client" }),
		awful.key({ modkey, "Shift" }, "Up", function()
			H.move_client_dir("up")
		end, { description = "move window up", group = "client" }),
		awful.key({ modkey, "Shift" }, "Down", function()
			H.move_client_dir("down")
		end, { description = "move window down", group = "client" })
	)
end
