-- ~/.config/awesome/input/keys/global/state.lua
local awful = require("awful")

local function suppress_center(sec)
	awesome.emit_signal("ui::suppress_center", sec or 0.2)
end

return function(modkey)
	return awful.util.table.join(
		awful.key({ modkey }, "f", function()
			local c = client.focus
			if c then
				c.fullscreen = not c.fullscreen
				c:raise()
			end
		end, { description = "toggle fullscreen", group = "client" }),

		-- Mod+m: Minimize, sonst Restore (Policy macht den Rest)
		awful.key({ modkey }, "m", function()
			local c = client.focus
			if c and not c.minimized then
				suppress_center(0.2)
				c.minimized = true
			else
				awesome.emit_signal("windowing::restore_request", awful.screen.focused())
			end
		end, { description = "minimize / restore", group = "client" }),

		awful.key({ modkey }, "t", function()
			local c = client.focus
			if c then
				awful.client.floating.toggle(c)
			end
		end, { description = "toggle floating", group = "client" })
	)
end
