-- ~/.config/awesome/input/keys/global/state.lua
local awful = require("awful")
local H = require("input.keys.helpers")

return function(modkey)
	return awful.util.table.join(
		awful.key({ modkey }, "f", function()
			local c = client.focus
			if c then
				c.fullscreen = not c.fullscreen
				c:raise()
			end
		end, { description = "toggle fullscreen", group = "client" }),

		-- Mod+m: normal -> pseudo-maximize
		--        pseudo-maximize -> minimize
		--        minimized -> restore + pseudo-maximize
		awful.key({ modkey }, "m", function()
			local c = client.focus
			if c and not c.minimized then
				if not c.maximized_fake then
					H.pseudo_maximize(c)
					c.maximized_fake = true
				else
					c.minimized = true
					c.maximized_fake = false
				end
			else
				local r = awful.client.restore()
				if r then
					H.pseudo_maximize(r)
					r.maximized_fake = true
					r:emit_signal("request::activate", "key.unminimize", { raise = true })
				end
			end
		end, { description = "toggle minimize/pseudo-maximize", group = "client" }),

		awful.key({ modkey }, "t", function()
			local c = client.focus
			if c then
				awful.client.floating.toggle(c)
			end
		end, { description = "toggle floating", group = "client" })
	)
end
