-- features/shell/menu/shared/placement.lua
local M = {}

-- Popup über oder unter der Bar positionieren (nutzt screen.workarea)
-- params = { position="bottom"|"top", gap=2, align="left"|"center"|"right" }
function M.above_bar(params)
	params = params or {}
	local pos = params.position or "bottom"
	local gap = params.gap or 2
	local align = params.align or "left"

	return function(p, s)
		local wa = s.workarea -- berücksichtigt Wibars/Docks automatisch

		-- X-Ausrichtung
		if align == "left" then
			p.x = wa.x
		elseif align == "right" then
			p.x = wa.x + wa.width - p.width
		else
			p.x = wa.x + math.floor((wa.width - p.width) / 2)
		end

		-- Y über oder unter der Bar
		if pos == "top" then
			-- Popup direkt UNTER der oberen Bar
			p.y = wa.y + gap
		else
			-- Popup direkt ÜBER der unteren Bar
			p.y = wa.y + wa.height - gap - p.height
		end
	end
end

-- Optional: X-Koordinate an Maus/Startbutton koppeln
function M.with_cursor_x(base_place_fn)
	return function(p, s, opts)
		base_place_fn(p, s)
		if opts and opts.coords and opts.coords.x then
			local g = s.geometry
			local x = math.max(g.x, math.min(opts.coords.x, g.x + g.width - p.width))
			p.x = x
		end
	end
end

return M
