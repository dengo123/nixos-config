local awful = require("awful")
local H = require("input.keys.helpers")

local function kbd_intent(ms)
	awesome.emit_signal("focus_policy::keyboard_intent", ms or 250)
end

return function(modkey)
	return awful.util.table.join(
		awful.key({ modkey, "Mod1" }, "Left", function()
			local t = H.scr_in_dir("left")
			if t then
				kbd_intent()
				awful.screen.focus(t)
			end
		end, { description = "focus screen left", group = "screen" }),

		awful.key({ modkey, "Mod1" }, "Right", function()
			local t = H.scr_in_dir("right")
			if t then
				kbd_intent()
				awful.screen.focus(t)
			end
		end, { description = "focus screen right", group = "screen" }),

		awful.key({ modkey, "Mod1" }, "Up", function()
			local t = H.scr_in_dir("up")
			if t then
				kbd_intent()
				awful.screen.focus(t)
			end
		end, { description = "focus screen up", group = "screen" }),

		awful.key({ modkey, "Mod1" }, "Down", function()
			local t = H.scr_in_dir("down")
			if t then
				kbd_intent()
				awful.screen.focus(t)
			end
		end, { description = "focus screen down", group = "screen" }),

		-- Fenster auf anderen Monitor (Fokus folgt meist)
		awful.key({ modkey, "Shift", "Mod1" }, "Left", function()
			if H and H.move_client_to_screen then
				kbd_intent()
				H.move_client_to_screen("left")
			end
		end, { description = "move window to screen left", group = "client" }),
		awful.key({ modkey, "Shift", "Mod1" }, "Right", function()
			if H and H.move_client_to_screen then
				kbd_intent()
				H.move_client_to_screen("right")
			end
		end, { description = "move window to screen right", group = "client" }),
		awful.key({ modkey, "Shift", "Mod1" }, "Up", function()
			if H and H.move_client_to_screen then
				kbd_intent()
				H.move_client_to_screen("up")
			end
		end, { description = "move window to screen up", group = "client" }),
		awful.key({ modkey, "Shift", "Mod1" }, "Down", function()
			if H and H.move_client_to_screen then
				kbd_intent()
				H.move_client_to_screen("down")
			end
		end, { description = "move window to screen down", group = "client" })
	)
end
