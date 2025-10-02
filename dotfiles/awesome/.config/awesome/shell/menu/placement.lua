-- ~/.config/awesome/shell/menu/placement.lua
local beautiful = require("beautiful")

local P = {}

-- Internals ---------------------------------------------------------------

local function bar_geometry(s)
	local wb = s and (s.mywibar or s.mywibox or s.wibar)
	if wb and wb.valid then
		return wb:geometry()
	end
	return nil
end

local function screen_geometry(s)
	if s and s.geometry then
		return s.geometry
	end
	return { x = 0, y = 0, width = 0, height = 0 }
end

-- Public API --------------------------------------------------------------

-- y über/unter der Bar (abhängig von beautiful.wibar_position).
-- total_h = Gesamthöhe des Menüs (items * item_height)
function P.y_over_bar(s, total_h)
	local gap = beautiful.menu_gap or 4
	local pos = beautiful.wibar_position or "bottom"
	local bg = bar_geometry(s)
	if bg then
		if pos == "bottom" then
			return bg.y - total_h - gap
		else -- "top"
			return bg.y + bg.height + gap
		end
	end
	-- Fallback ohne Bar: am unteren Bildschirmrand mit Abstand
	local sg = screen_geometry(s)
	local wh = tonumber(beautiful.wibar_height) or 28
	return sg.y + math.max(0, sg.height - total_h - (wh + gap))
end

-- x linksbündig an einer expliziten Ankerkante (Tabs).
-- Nutzt globalen Offset beautiful.menu_x_offset (kann negativ sein).
function P.x_left_from_anchor(_s, x_left)
	local pad = beautiful.menu_x_padding or 8
	local off = beautiful.menu_x_offset or 0
	return (x_left or 0) + pad + off
end

-- x linksbündig an der Bar (Start).
-- Nutzt globalen Offset beautiful.menu_x_offset (kann negativ sein).
function P.x_left_on_bar(s)
	local pad = beautiful.menu_x_padding or 8
	local off = beautiful.menu_x_offset or 0
	local bg = bar_geometry(s)
	local base = bg and bg.x or screen_geometry(s).x
	return base + pad + off
end

return P
