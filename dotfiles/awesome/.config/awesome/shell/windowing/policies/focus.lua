-- ~/.config/awesome/shell/windowing/policies/focus.lua
local gears = require("gears")
local awful = require("awful")

local F = {}
F.__index = F

function F.init(o)
	F.sloppy_focus = (o.sloppy_focus ~= false)
	F.center_mouse_on_focus = (o.center_mouse_on_focus ~= false)
	F.raise_on_mouse_focus = o.raise_on_mouse_focus or false
	F.block_ms = o.block_ms or 150

	-- Nur wenn kürzlich Tastatur-Intent gemeldet wurde, darf zentriert werden
	F._kbd_recent = false
	awesome.connect_signal("focus_policy::keyboard_intent", function(ms)
		F._kbd_recent = true
		gears.timer.start_new((ms or F.block_ms) / 1000, function()
			F._kbd_recent = false
			return false
		end)
	end)

	-- Maus-getriebene Zentrierung komplett unterbinden
	F._suppress_center = false
	awesome.connect_signal("ui::suppress_center", function(sec)
		F._suppress_center = true
		gears.timer.start_new((sec or 0.15), function()
			F._suppress_center = false
			return false
		end)
	end)
end

-- Sloppy focus (Maus → Fokus), ohne Zentrieren
function F:on_mouse_enter(c)
	if not F.sloppy_focus then
		return
	end
	if not awful.client.focus.filter(c) or c.minimized or not c:isvisible() then
		return
	end
	client.focus = c
	if F.raise_on_mouse_focus then
		c:raise()
	end
	-- KEIN keyboard_intent, also kein Zentrieren im on_focus
end

-- Zentriere nur nach Tastatur-Intent
function F:on_focus(c)
	if not F.center_mouse_on_focus then
		return
	end
	if not F._kbd_recent or F._suppress_center then
		return
	end
	if not (c and c.valid) or c.minimized or not c:isvisible() then
		return
	end
	gears.timer.delayed_call(function()
		local g = c:geometry()
		mouse.coords({ x = g.x + math.floor(g.width / 2), y = g.y + math.floor(g.height / 2) })
	end)
end

return F
