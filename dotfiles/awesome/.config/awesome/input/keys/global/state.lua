-- ~/.config/awesome/input/keys/global/state.lua
local awful = require("awful")
local naughty = require("naughty")

-- Merker, ob Idle-Dimmen aktuell blockiert ist
local idle_block = false

local function apply_idle_block(on)
	if on then
		awful.spawn.with_shell("xset s off -dpms; xset s noblank")
	else
		awful.spawn.with_shell("xset s on +dpms")
	end
	idle_block = on
end

local function suppress_center(sec)
	awesome.emit_signal("ui::suppress_center", sec or 0.2)
end

return function(modkey)
	return awful.util.table.join(
		-- Mod+F: Fullscreen togglen UND Idle-Dimmen passend setzen
		awful.key({ modkey }, "f", function()
			local c = client.focus
			if c then
				local entering_fullscreen = not c.fullscreen
				c.fullscreen = entering_fullscreen
				c:raise()

				-- Beim Eintritt in Fullscreen: Dimmen blockieren; beim Verlassen: wieder erlauben
				apply_idle_block(entering_fullscreen)

				naughty.notify({
					title = "Fullscreen & Idle",
					text = entering_fullscreen and "Fullscreen AN · Idle-Dimmen blockiert"
						or "Fullscreen AUS · Idle-Dimmen erlaubt",
					timeout = 2,
				})
			else
				-- Kein fokussiertes Fenster: nur Idle-Block togglen
				apply_idle_block(not idle_block)
				naughty.notify({
					title = "Idle-Dimmen",
					text = idle_block and "Blockiert" or "Erlaubt",
					timeout = 2,
				})
			end
		end, { description = "toggle fullscreen + idle block", group = "client" }),

		-- Mod+m: Minimize / Restore (deins)
		awful.key({ modkey }, "m", function()
			local c = client.focus
			if c and not c.minimized then
				suppress_center(0.2)
				c.minimized = true
			else
				awesome.emit_signal("windowing::restore_request", awful.screen.focused())
			end
		end, { description = "minimize / restore", group = "client" }),

		-- Mod+t: Floating togglen (deins)
		awful.key({ modkey }, "t", function()
			local c = client.focus
			if c then
				awful.client.floating.toggle(c)
			end
		end, { description = "toggle floating", group = "client" })
	)
end
