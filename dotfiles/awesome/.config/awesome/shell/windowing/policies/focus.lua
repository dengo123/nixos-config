-- windowing/policies/focus.lua
local gears = require("gears")
local awful = require("awful")

local F = {}
F.__index = F

function F.init(o)
	F.sloppy_focus = (o.sloppy_focus ~= false)
	F.center_mouse_on_focus = (o.center_mouse_on_focus ~= false)
	F.raise_on_mouse_focus = o.raise_on_mouse_focus or false
	F.block_ms = o.block_ms or 150

	F._mouse_recent = false
	awesome.connect_signal("focus_policy::mouse_recent", function(ms)
		F._mouse_recent = true
		gears.timer.start_new((ms or F.block_ms) / 1000, function()
			F._mouse_recent = false
			return false
		end)
	end)

	F._suppress_center = false
	awesome.connect_signal("ui::suppress_center", function(sec)
		F._suppress_center = true
		gears.timer.start_new((sec or 0.15), function()
			F._suppress_center = false
			return false
		end)
	end)
end

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
	awesome.emit_signal("focus_policy::mouse_recent", F.block_ms)
end

function F:on_focus(c)
	if not F.center_mouse_on_focus then
		return
	end
	if F._mouse_recent or F._suppress_center then
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
