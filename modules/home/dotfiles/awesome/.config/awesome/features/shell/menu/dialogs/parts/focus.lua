local awful = require("awful")

local Focus = {}

-- items: Liste von Widgets, die :set_focus(on, th) und :activate() besitzen
function Focus.attach(handle, items, th)
	if not items or #items == 0 then
		return
	end
	local i = 1
	local function hi(idx, on)
		local w = items[idx]
		if w and w.set_focus then
			w:set_focus(on, th)
		end
	end
	hi(i, true)

	local id = awful.keygrabber.run(function(_, key, ev)
		if ev == "release" then
			return
		end
		if key == "Right" then
			hi(i, false)
			i = math.min(#items, i + 1)
			hi(i, true)
		elseif key == "Left" then
			hi(i, false)
			i = math.max(1, i - 1)
			hi(i, true)
		elseif key == "Return" or key == "KP_Enter" then
			local w = items[i]
			if w and w.activate then
				w:activate()
			end
		end
	end)

	-- Grabber stoppen, wenn Popup zugeht
	local old_close = handle.close
	handle.close = function(...)
		pcall(awful.keygrabber.stop, id)
		return old_close(...)
	end
end

return Focus
