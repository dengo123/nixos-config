-- ~/.config/awesome/input/keys/global/screens.lua
local awful = require("awful")
local H = require("input.keys.helpers")

return function(modkey)
	return awful.util.table.join(
		awful.key({ modkey, "Mod1" }, "Left", function()
			local t = H.scr_in_dir("left")
			if t then
				awful.screen.focus(t)
			end
		end, { description = "focus screen left", group = "screen" }),
		awful.key({ modkey, "Mod1" }, "Right", function()
			local t = H.scr_in_dir("right")
			if t then
				awful.screen.focus(t)
			end
		end, { description = "focus screen right", group = "screen" }),
		awful.key({ modkey, "Mod1" }, "Up", function()
			local t = H.scr_in_dir("up")
			if t then
				awful.screen.focus(t)
			end
		end, { description = "focus screen up", group = "screen" }),
		awful.key({ modkey, "Mod1" }, "Down", function()
			local t = H.scr_in_dir("down")
			if t then
				awful.screen.focus(t)
			end
		end, { description = "focus screen down", group = "screen" })
	)
end
