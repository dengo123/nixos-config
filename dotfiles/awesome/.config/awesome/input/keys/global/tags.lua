-- ~/.config/awesome/input/keys/global/tags.lua
local awful = require("awful")
local H = require("input.keys.helpers")
local tags = require("shell.workspaces.core")

return function(modkey)
	return awful.util.table.join(
		-- Tag wechseln
		awful.key({ modkey, "Control" }, "Right", function()
			H.view_tag_idx(1)
		end, { description = "next tag (on screen)", group = "tag" }),
		awful.key({ modkey, "Control" }, "Left", function()
			H.view_tag_idx(-1)
		end, { description = "prev tag (on screen)", group = "tag" }),

		-- Tag verschieben (Screen)
		awful.key({ modkey, "Control", "Mod1" }, "Left", function()
			H.move_tag_to_screen("left")
		end, { description = "move tag to screen left", group = "tag" }),
		awful.key({ modkey, "Control", "Mod1" }, "Right", function()
			H.move_tag_to_screen("right")
		end, { description = "move tag to screen right", group = "tag" }),
		awful.key({ modkey, "Control", "Mod1" }, "Up", function()
			H.move_tag_to_screen("up")
		end, { description = "move tag to screen up", group = "tag" }),
		awful.key({ modkey, "Control", "Mod1" }, "Down", function()
			H.move_tag_to_screen("down")
		end, { description = "move tag to screen down", group = "tag" }),

		-- Fenster zwischen Tags (Fokus folgt)
		awful.key({ modkey, "Control", "Shift" }, "Right", function()
			H.move_client_to_neighbor_tag(1, true)
		end, { description = "move window to next tag (focus follows)", group = "client" }),
		awful.key({ modkey, "Control", "Shift" }, "Left", function()
			H.move_client_to_neighbor_tag(-1, true)
		end, { description = "move window to prev tag (focus follows)", group = "client" }),

		-- Dynamische Tags
		awful.key({ modkey }, "n", function()
			tags.add()
		end, { description = "new tag (focus)", group = "tag" }),
		awful.key({ modkey, "Shift" }, "n", function()
			tags.add_silent()
		end, { description = "new tag (silent)", group = "tag" }),
		awful.key({ modkey }, "c", function()
			tags.delete_current()
		end, { description = "close current tag (soft)", group = "tag" }),
		awful.key({ modkey, "Shift" }, "c", function()
			tags.delete_current_force()
		end, { description = "force close current tag", group = "tag" })
	)
end
