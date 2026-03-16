-- ~/.config/awesome/shell/windowing/policies/focus.lua

local gears = require("gears")
local awful = require("awful")

local F = {}
F.__index = F

-- =====================================================================
-- Defaults
-- =====================================================================

F.sloppy = true
F.center_mouse = true
F.raise_on_mouse = false
F.block_ms = 150

-- Intern
F._kbd_recent = false
F._suppress_center = false

-- =====================================================================
-- Init
-- =====================================================================

function F.init(o)
	o = o or {}

	F.sloppy = (o.sloppy ~= false)
	F.center_mouse = (o.center_mouse ~= false)
	F.raise_on_mouse = (o.raise_on_mouse == true)
	F.block_ms = tonumber(o.block_ms) or 150

	-- Keyboard intent:
	-- Nur wenn kurz zuvor dieses Signal kam, darf on_focus den Cursor zentrieren.
	awesome.connect_signal("focus_policy::keyboard_intent", function(ms)
		F._kbd_recent = true
		gears.timer.start_new(((ms or F.block_ms) / 1000), function()
			F._kbd_recent = false
			return false
		end)
	end)

	-- Temporäre Unterdrückung der Maus-Zentrierung
	awesome.connect_signal("ui::suppress_center", function(sec)
		F._suppress_center = true
		gears.timer.start_new((sec or 0.15), function()
			F._suppress_center = false
			return false
		end)
	end)
end

-- =====================================================================
-- Mouse / Focus Handlers
-- =====================================================================

-- Sloppy focus: Fokus folgt der Maus, ohne Zentrierung auszulösen
function F:on_mouse_enter(c)
	if not F.sloppy then
		return
	end
	if not (c and c.valid) then
		return
	end
	if not awful.client.focus.filter(c) or c.minimized or not c:isvisible() then
		return
	end

	client.focus = c

	if F.raise_on_mouse then
		c:raise()
	end
end

-- Cursor nur nach Tastatur-Intent auf das fokussierte Fenster zentrieren
function F:on_focus(c)
	if not F.center_mouse then
		return
	end
	if not F._kbd_recent or F._suppress_center then
		return
	end
	if not (c and c.valid) or c.minimized or not c:isvisible() then
		return
	end

	gears.timer.delayed_call(function()
		if not (c and c.valid) or c.minimized or not c:isvisible() then
			return
		end

		local g = c:geometry()
		mouse.coords({
			x = g.x + math.floor(g.width / 2),
			y = g.y + math.floor(g.height / 2),
		})
	end)
end

return F
