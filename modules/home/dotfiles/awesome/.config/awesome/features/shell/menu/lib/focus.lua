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

--- Linearer Fokus über eine Liste von Items (je Item: :set_focus(on, th), :activate()).
function Focus.attach(items, th, opts)
	opts = opts or {}
	local keys = norm_keys(opts.keys)
	local n = #items
	if n == 0 then
		return function() end
	end

	local i = 1

	local function hi(idx, on)
		local w = items[idx]
		if w and w.set_focus then
			pcall(w.set_focus, w, on, th)
		end
	end

	hi(i, true)

	-- **Kompatible** Keygrabber-Form (run)
	local kg_id = awful.keygrabber.run(function(_, key, ev)
		if ev == "release" then
			return
		end

		if key == keys.left or key == keys.up then
			hi(i, false)
			i = math.max(1, i - 1)
			hi(i, true)
		elseif key == keys.right or key == keys.down then
			hi(i, false)
			i = math.min(n, i + 1)
			hi(i, true)
		elseif key == keys.cancel then
			-- Dialog/Popup schließen, falls gegeben
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
		for idx = 1, n do
			pcall(hi, idx, false)
		end
	end
end

return Focus
