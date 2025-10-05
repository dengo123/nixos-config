-- ~/.config/awesome/shell/windowing/policies/focus.lua
-- Maus/Fokus-Policy:
-- - Sloppy Focus (optional): Fokus folgt der Maus, aber ohne Maus zu bewegen
-- - Zentriere Cursor bei Fokuswechsel NUR nach "keyboard intent"
--   (über awesome.emit_signal("focus_policy::keyboard_intent", ms))
-- - Unterdrücke zentrieren temporär via "ui::suppress_center" (z. B. bei Tag-Wechsel)
--
-- Verwendung:
--   local Focus = require("shell.windowing.policies.focus")
--   Focus.init{
--     sloppy_focus = true,
--     center_mouse_on_focus = true,
--     raise_on_mouse_focus = false,
--     block_ms = 150, -- wie lange "keyboard intent" als frisch gilt
--   }
--
-- Danach die Hooks an den richtigen Stellen registrieren, z. B.:
--   client.connect_signal("mouse::enter", function(c) Focus:on_mouse_enter(c) end)
--   client.connect_signal("focus", function(c) Focus:on_focus(c) end)

local gears = require("gears")
local awful = require("awful")

local F = {}
F.__index = F

-- Default-Konfiguration
F.sloppy_focus = true
F.center_mouse_on_focus = true
F.raise_on_mouse_focus = false
F.block_ms = 150

-- Intern: States
F._kbd_recent = false -- wurde kürzlich per Tastatur navigiert?
F._suppress_center = false -- zentrieren temporär unterdrücken?

function F.init(o)
	o = o or {}
	F.sloppy_focus = (o.sloppy_focus ~= false)
	F.center_mouse_on_focus = (o.center_mouse_on_focus ~= false)
	F.raise_on_mouse_on_focus = o.raise_on_mouse_focus or false
	F.block_ms = tonumber(o.block_ms) or 150

	-- „Keyboard intent“: nur wenn das Signal kurz zuvor gesendet wurde, zentrieren wir.
	awesome.connect_signal("focus_policy::keyboard_intent", function(ms)
		F._kbd_recent = true
		gears.timer.start_new(((ms or F.block_ms) / 1000), function()
			F._kbd_recent = false
			return false
		end)
	end)

	-- Zentrierung kurzzeitig unterdrücken (z. B. beim Tag-Wechsel)
	awesome.connect_signal("ui::suppress_center", function(sec)
		F._suppress_center = true
		gears.timer.start_new((sec or 0.15), function()
			F._suppress_center = false
			return false
		end)
	end)
end

-- Sloppy focus (Fokus folgt Maus), ohne die Maus zu bewegen
function F:on_mouse_enter(c)
	if not F.sloppy_focus then
		return
	end
	if not awful.client.focus.filter(c) or c.minimized or not c:isvisible() then
		return
	end
	client.focus = c
	if F.raise_on_mouse_on_focus then
		c:raise()
	end
	-- Wichtig: KEIN keyboard_intent hier – Mausbewegung soll NICHT zentrieren auslösen
end

-- Cursor zentrieren, aber nur nach Tastatur-Intent und wenn nicht unterdrückt
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

	-- Zentriere leicht verzögert auf das fokussierte Fenster
	gears.timer.delayed_call(function()
		-- Hinweis: wir zentrieren absichtlich unabhängig davon, ob der Screen gewechselt hat.
		-- Tag-Wechsel, bei denen der Cursor NICHT wandern soll, sollten zuvor „ui::suppress_center“
		-- auslösen (siehe features/workspaces/tags/focus_policy.lua).
		local g = c:geometry()
		mouse.coords({
			x = g.x + math.floor(g.width / 2),
			y = g.y + math.floor(g.height / 2),
		})
	end)
end

return F
