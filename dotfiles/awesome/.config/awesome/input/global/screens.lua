-- ~/.config/awesome/input/global/screens.lua
local awful = require("awful")

local function kbd_intent(ms)
	awesome.emit_signal("focus_policy::keyboard_intent", ms or 250)
end

local function call_move(actions, dir)
	if not (actions and type(actions.move_client_to_screen) == "function") then
		return
	end

	kbd_intent()
	actions.move_client_to_screen(dir)
end

return function(modkey, actions)
	assert(type(actions) == "table", "keys.global.screens: actions fehlt/ungueltig")

	return awful.util.table.join(
		awful.key({ modkey, "Mod1" }, "Left", function()
			local t = actions.scr_in_dir and actions.scr_in_dir("left") or nil
			if t then
				kbd_intent()
				awful.screen.focus(t)
			end
		end, { description = "Focus Screen Left", group = "Screen" }),

		awful.key({ modkey, "Mod1" }, "Right", function()
			local t = actions.scr_in_dir and actions.scr_in_dir("right") or nil
			if t then
				kbd_intent()
				awful.screen.focus(t)
			end
		end, { description = "Focus Screen Right", group = "Screen" }),

		awful.key({ modkey, "Mod1" }, "Up", function()
			local t = actions.scr_in_dir and actions.scr_in_dir("up") or nil
			if t then
				kbd_intent()
				awful.screen.focus(t)
			end
		end, { description = "Focus Screen Up", group = "Screen" }),

		awful.key({ modkey, "Mod1" }, "Down", function()
			local t = actions.scr_in_dir and actions.scr_in_dir("down") or nil
			if t then
				kbd_intent()
				awful.screen.focus(t)
			end
		end, { description = "Focus Screen Down", group = "Screen" }),

		awful.key({ modkey, "Shift", "Mod1" }, "Left", function()
			call_move(actions, "left")
		end, { description = "Move Window To Screen Left", group = "Client" }),

		awful.key({ modkey, "Shift", "Mod1" }, "Right", function()
			call_move(actions, "right")
		end, { description = "Move Window To Screen Right", group = "Client" }),

		awful.key({ modkey, "Shift", "Mod1" }, "Up", function()
			call_move(actions, "up")
		end, { description = "Move Window To Screen Up", group = "Client" }),

		awful.key({ modkey, "Shift", "Mod1" }, "Down", function()
			call_move(actions, "down")
		end, { description = "Move Window To Screen Down", group = "Client" })
	)
end
