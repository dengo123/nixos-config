-- input/keys/global/screens.lua
local awful = require("awful")

local function kbd_intent(ms)
	awesome.emit_signal("focus_policy::keyboard_intent", ms or 250)
end

return function(modkey, actions)
	assert(type(actions) == "table", "keys.global.screens: actions fehlt/ungueltig")

	return awful.util.table.join(
		awful.key({ modkey, "Mod1" }, "Left", function()
			local t = actions.scr_in_dir("left")
			if t then
				kbd_intent()
				awful.screen.focus(t)
			end
		end, { description = "focus screen left", group = "screen" }),

		awful.key({ modkey, "Mod1" }, "Right", function()
			local t = actions.scr_in_dir("right")
			if t then
				kbd_intent()
				awful.screen.focus(t)
			end
		end, { description = "focus screen right", group = "screen" }),

		awful.key({ modkey, "Mod1" }, "Up", function()
			local t = actions.scr_in_dir("up")
			if t then
				kbd_intent()
				awful.screen.focus(t)
			end
		end, { description = "focus screen up", group = "screen" }),

		awful.key({ modkey, "Mod1" }, "Down", function()
			local t = actions.scr_in_dir("down")
			if t then
				kbd_intent()
				awful.screen.focus(t)
			end
		end, { description = "focus screen down", group = "screen" }),

		awful.key({ modkey, "Shift", "Mod1" }, "Left", function()
			kbd_intent()
			actions.move_client_to_screen("left")
		end, { description = "move window to screen left", group = "client" }),

		awful.key({ modkey, "Shift", "Mod1" }, "Right", function()
			kbd_intent()
			actions.move_client_to_screen("right")
		end, { description = "move window to screen right", group = "client" }),

		awful.key({ modkey, "Shift", "Mod1" }, "Up", function()
			kbd_intent()
			actions.move_client_to_screen("up")
		end, { description = "move window to screen up", group = "client" }),

		awful.key({ modkey, "Shift", "Mod1" }, "Down", function()
			kbd_intent()
			actions.move_client_to_screen("down")
		end, { description = "move window to screen down", group = "client" })
	)
end
