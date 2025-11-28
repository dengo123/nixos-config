local awful = require("awful")
local H = require("input.keys.helpers")
local tags = require("shell.workspaces")

local function kbd_intent(ms)
	awesome.emit_signal("focus_policy::keyboard_intent", ms or 250)
end

return function(modkey)
	return awful.util.table.join(
		-- Tag wechseln
		awful.key({ modkey, "Control" }, "Right", function()
			kbd_intent()
			H.view_tag_idx(1)
		end, { description = "next tag (on screen)", group = "tag" }),
		awful.key({ modkey, "Control" }, "Left", function()
			kbd_intent()
			H.view_tag_idx(-1)
		end, { description = "prev tag (on screen)", group = "tag" }),

		-- Aktuellen Tag in der Liste verschieben (Reihenfolge auf dem Screen)
		awful.key({ modkey, "Control" }, "Up", function()
			kbd_intent()
			local s = awful.screen.focused()
			if not s then
				return
			end
			local t = s.selected_tag
			if not t then
				return
			end

			local idx = t.index or 1
			if idx <= 1 then
				return
			end

			-- Tag eine Position nach oben schieben
			awful.tag.move(idx - 1, t)
			awful.tag.viewonly(t) -- sicherstellen, dass wir auf dem Tag bleiben
		end, { description = "move current tag up in list", group = "tag" }),

		awful.key({ modkey, "Control" }, "Down", function()
			kbd_intent()
			local s = awful.screen.focused()
			if not s then
				return
			end
			local t = s.selected_tag
			if not t then
				return
			end

			local idx = t.index or 1
			local all = s.tags or {}
			if idx >= #all then
				return
			end

			-- Tag eine Position nach unten schieben
			awful.tag.move(idx + 1, t)
			awful.tag.viewonly(t)
		end, { description = "move current tag down in list", group = "tag" }),

		-- Tag verschieben (Screen)
		awful.key({ modkey, "Control", "Mod1" }, "Left", function()
			kbd_intent()
			H.move_tag_to_screen("left")
		end, { description = "move tag to screen left", group = "tag" }),
		awful.key({ modkey, "Control", "Mod1" }, "Right", function()
			kbd_intent()
			H.move_tag_to_screen("right")
		end, { description = "move tag to screen right", group = "tag" }),
		awful.key({ modkey, "Control", "Mod1" }, "Up", function()
			kbd_intent()
			H.move_tag_to_screen("up")
		end, { description = "move tag to screen up", group = "tag" }),
		awful.key({ modkey, "Control", "Mod1" }, "Down", function()
			kbd_intent()
			H.move_tag_to_screen("down")
		end, { description = "move tag to screen down", group = "tag" }),

		-- Fenster zwischen Tags (Fokus folgt)
		awful.key({ modkey, "Control", "Shift" }, "Right", function()
			kbd_intent()
			H.move_client_to_neighbor_tag(1, true)
		end, { description = "move window to next tag (focus follows)", group = "client" }),
		awful.key({ modkey, "Control", "Shift" }, "Left", function()
			kbd_intent()
			H.move_client_to_neighbor_tag(-1, true)
		end, { description = "move window to prev tag (focus follows)", group = "client" }),

		-- Dynamische Tags
		awful.key({ modkey }, "n", function()
			kbd_intent()
			tags.add()
		end, { description = "new tag (focus)", group = "tag" }),
		awful.key({ modkey, "Shift" }, "n", function()
			kbd_intent()
			tags.add_silent()
		end, { description = "new tag (silent)", group = "tag" }),
		awful.key({ modkey }, "c", function()
			kbd_intent()
			tags.delete_current()
		end, { description = "close current tag (soft)", group = "tag" }),
		awful.key({ modkey, "Shift" }, "c", function()
			kbd_intent()
			tags.delete_current_force()
		end, { description = "force close current tag", group = "tag" })
	)
end
