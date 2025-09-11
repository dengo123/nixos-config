-- ~/.config/awesome/input/keys/global/state.lua
local awful = require("awful")

return function(modkey)
	return awful.util.table.join(
		awful.key({ modkey }, "f", function()
			local c = client.focus
			if c then
				c.fullscreen = not c.fullscreen
				c:raise()
			end
		end, { description = "toggle fullscreen", group = "client" }),

		awful.key({ modkey }, "m", function()
			local c = client.focus
			if c then
				c.maximized = not c.maximized
				c:raise()
			end
		end, { description = "toggle maximize", group = "client" }),

		awful.key({ modkey }, "t", function()
			local c = client.focus
			if c then
				awful.client.floating.toggle(c)
			end
		end, { description = "toggle floating", group = "client" })
	)
end
