-- ~/.config/awesome/features/shell/menu/dialogs/parts/focus.lua
local awful = require("awful")

local Focus = {}

-- items: Widgets mit :set_focus(on, th) und :activate()
-- return: stop() -> Grabber sauber beenden + Fokus-Optik resetten
function Focus.attach(handle, items, th)
	if not items or #items == 0 then
		return function() end
	end

	local n = #items
	local i = 1

	local function set(idx, on)
		local w = items[idx]
		if w and w.set_focus then
			w:set_focus(on, th)
		end
	end

	local function move(delta) -- delta = 1 oder -1, mit Wrap-Around
		set(i, false)
		i = ((i - 1 + delta) % n) + 1
		set(i, true)
	end

	-- initialer Fokus
	set(i, true)

	-- funktionale Keygrabber-API (Awesome 4.3 kompatibel)
	local id = awful.keygrabber.run(function(_, key, event)
		if event == "release" then
			return
		end

		if key == "Right" then
			move(1)
		elseif key == "Left" then
			move(-1)
		elseif key == "Tab" then -- optional vor
			move(1)
		elseif key == "ISO_Left_Tab" then -- optional zurück (Shift+Tab auf manchen Layouts)
			move(-1)
		elseif key == "Return" or key == "KP_Enter" then
			local w = items[i]
			if w and w.activate then
				w:activate()
			end
		elseif key == "Escape" then
			if handle and handle.close then
				handle.close()
			end
		end
	end)

	-- Cleanup-Funktion
	local function stop()
		if id then
			pcall(awful.keygrabber.stop, id)
		end
		for idx = 1, n do
			set(idx, false)
		end
	end

	-- Stop beim Schließen des Popups
	local old_close = handle.close
	handle.close = function(...)
		pcall(stop)
		return old_close(...)
	end

	return stop
end

return Focus
