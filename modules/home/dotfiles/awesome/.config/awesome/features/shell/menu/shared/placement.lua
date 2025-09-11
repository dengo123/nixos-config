-- features/shell/menu/shared/placement.lua
local M = {}

-- Popup über/unter einer Bar
function M.above_bar(params)
	params = params or {}
	local pos = params.position or "bottom" -- "bottom" | "top"
	local bh = params.bar_height or 32 -- Zahl oder function(s)->zahl
	local gap = params.gap or 2
	local align = params.align or "left" -- "left" | "right" | "center"

	return function(p, s)
		local g = s.geometry
		local bar_h = (type(bh) == "function") and bh(s) or bh

		-- X-Ausrichtung
		if align == "left" then
			p.x = g.x
		elseif align == "right" then
			p.x = g.x + g.width - p.width
		else
			p.x = g.x + math.floor((g.width - p.width) / 2)
		end

		-- Y über/unter Bar
		if pos == "top" then
			p.y = g.y + bar_h + gap
		else
			p.y = g.y + g.height - bar_h - gap - p.height
		end
	end
end

-- Optionaler Wrapper: X-Koordinate an Maus/Startbutton binden
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
