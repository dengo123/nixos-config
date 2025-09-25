-- ~/.config/awesome/features/shell/menu/lib/focus.lua
local awful = require("awful")

local Focus = {}

local function norm_keys(k)
	return {
		left = (k and k.left) or "Left",
		right = (k and k.right) or "Right",
		up = (k and k.up) or "Up",
		down = (k and k.down) or "Down",
		ok = (k and k.ok) or { "Return", "KP_Enter" },
		cancel = (k and k.cancel) or "Escape",
	}
end

--- Linearer Fokus über eine Liste von Items.
--- Erwartet je Item: :set_focus(on, th), :activate(), optional .mouse_enter_target (Widget)
function Focus.attach(items, th, opts)
	opts = opts or {}
	local keys = norm_keys(opts.keys)
	local n = #items
	if n == 0 then
		return function() end
	end

	local i = 1
	local enter_handlers = {} -- [{target=..., cb=...}, ...]

	local function hi(idx, on)
		local w = items[idx]
		if w and w.set_focus then
			pcall(w.set_focus, w, on, th)
		end
	end

	local function set_index(new_i)
		new_i = math.max(1, math.min(n, new_i))
		if new_i == i then
			return
		end
		hi(i, false)
		i = new_i
		hi(i, true)
	end

	-- initialer Fokus
	hi(i, true)

	-- Maus folgt Fokus (default: an). Nutzen, falls vorhanden: w.mouse_enter_target
	if opts.mouse_follow ~= false then
		for idx, w in ipairs(items) do
			local target = (w and w.mouse_enter_target) or w
			if target and target.connect_signal then
				local on_enter = function()
					set_index(idx)
				end
				target:connect_signal("mouse::enter", on_enter)
				enter_handlers[idx] = { target = target, cb = on_enter }
			end
		end
	end

	-- Keygrabber
	local kg_id = awful.keygrabber.run(function(_, key, ev)
		if ev == "release" then
			return
		end

		if key == keys.left or key == keys.up then
			set_index(i - 1)
		elseif key == keys.right or key == keys.down then
			set_index(i + 1)
		elseif key == keys.cancel then
			if opts.handle and opts.handle.close then
				pcall(function()
					opts.handle:close()
				end)
			elseif opts.handle and opts.handle.hide then
				pcall(function()
					opts.handle:hide()
				end)
			end
		else
			local oks = type(keys.ok) == "table" and keys.ok or { keys.ok }
			for _, enter in ipairs(oks) do
				if key == enter then
					local w = items[i]
					if w and w.activate then
						pcall(w.activate, w)
					end
					break
				end
			end
		end
	end)

	-- Stop/Cleanup
	return function()
		if kg_id then
			pcall(awful.keygrabber.stop, kg_id)
			kg_id = nil
		end
		-- Mouse-Handler sauber entfernen
		for idx, h in ipairs(enter_handlers) do
			if h and h.target and h.cb then
				pcall(h.target.disconnect_signal, h.target, "mouse::enter", h.cb)
			end
			enter_handlers[idx] = nil
		end
		-- Fokus visual clearen
		for idx = 1, n do
			pcall(hi, idx, false)
		end
	end
end

return Focus
