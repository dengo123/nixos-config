-- ~/.config/awesome/input/global/screens.lua
local awful = require("awful")
local gears = require("gears")

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

local function focus_screen(actions, dir)
	if not (actions and type(actions.scr_in_dir) == "function") then
		return
	end

	local target = actions.scr_in_dir(dir)
	if target then
		kbd_intent()
		awful.screen.focus(target)
	end
end

local function has_screen_actions(actions)
	return type(actions) == "table"
		and (type(actions.scr_in_dir) == "function" or type(actions.move_client_to_screen) == "function")
end

return function(modkey, actions)
	if not has_screen_actions(actions) then
		return gears.table.join()
	end

	return gears.table.join(
		awful.key({ modkey, "Mod1" }, "Left", function()
			focus_screen(actions, "left")
		end, { description = "Focus Screen Left", group = "Screen" }),

		awful.key({ modkey, "Mod1" }, "Right", function()
			focus_screen(actions, "right")
		end, { description = "Focus Screen Right", group = "Screen" }),

		awful.key({ modkey, "Mod1" }, "Up", function()
			focus_screen(actions, "up")
		end, { description = "Focus Screen Up", group = "Screen" }),

		awful.key({ modkey, "Mod1" }, "Down", function()
			focus_screen(actions, "down")
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
