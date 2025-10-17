local awful = require("awful")

return function(modkey)
	return awful.util.table.join(

		-- Mod+F: Natives Fullscreen, KEIN Dimmen
		awful.key({ modkey }, "f", function()
			local c = client.focus
			if not c then
				return
			end
			local entering = not c.fullscreen
			-- per-Client Flag explizit AUS
			c._fullscreen_dim = false
			c.fullscreen = entering
			c:raise()
			-- Beim Verlassen Fullscreen Flag weg
			if not entering then
				c._fullscreen_dim = nil
			end
		end, { description = "toggle fullscreen (native)", group = "client" }),

		-- Mod+Shift+F: Fullscreen mit Dim/Inhibit
		awful.key({ modkey, "Shift" }, "f", function()
			local c = client.focus
			if not c then
				return
			end
			local entering = not c.fullscreen
			-- per-Client Flag explizit AN
			c._fullscreen_dim = true
			c.fullscreen = entering
			c:raise()
			-- Beim Verlassen Fullscreen Flag weg
			if not entering then
				c._fullscreen_dim = nil
			end
		end, { description = "toggle fullscreen (dim other screens)", group = "client" }),

		-- ... (deine übrigen Keys: m, t, etc.)
		awful.key({ modkey }, "m", function()
			local c = client.focus
			if c and not c.minimized then
				awesome.emit_signal("ui::suppress_center", 0.2)
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
