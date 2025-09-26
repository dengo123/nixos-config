-- ~/.config/awesome/features/shell/menu/lib/focus.lua
local awful = require("awful")

local Focus = {}

-- Normalisiere Key-Bindings mit Defaults
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

---------------------------------------------------------------------
-- Linearer Fokus über eine Liste (jedes Item: :set_focus(on, th), :activate()).
---------------------------------------------------------------------
function Focus.attach(items, th, opts)
	opts = opts or {}
	local keys = norm_keys(opts.keys)
	local n = #items
	if n == 0 then
		return function() end
	end

	local i = 1
	local mouse_follow = (opts.mouse_follow ~= false)
	local enter_handlers = {}

	local function hi(idx, on)
		local w = items[idx]
		if w and w.set_focus then
			pcall(w.set_focus, w, on, th)
		end
	end

	local function set_index(new_i)
		if new_i == i then
			return
		end
		hi(i, false)
		i = math.max(1, math.min(n, new_i))
		hi(i, true)
	end

	-- initialer Fokus
	hi(i, true)

	-- Maus folgt Fokus (optional)
	if mouse_follow then
		for idx, w in ipairs(items) do
			if w and w.connect_signal then
				local target = w.mouse_enter_target or w
				if target and target.connect_signal then
					local on_enter = function()
						set_index(idx)
					end
					target:connect_signal("mouse::enter", on_enter)
					enter_handlers[idx] = { obj = target, cb = on_enter }
				end
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

	-- Cleanup
	return function()
		if kg_id then
			pcall(awful.keygrabber.stop, kg_id)
			kg_id = nil
		end
		for idx, rec in ipairs(enter_handlers) do
			if rec and rec.obj and rec.cb then
				pcall(rec.obj.disconnect_signal, rec.obj, "mouse::enter", rec.cb)
			end
			enter_handlers[idx] = nil
		end
		for idx = 1, n do
			pcall(hi, idx, false)
		end
	end
end

---------------------------------------------------------------------
-- Drei-Zonen-Fokus: linke Spalte, rechte Spalte, Power-Leiste
-- - ↑/↓: innerhalb der aktiven Spalte; am Ende ↓ → Power
-- - ←/→: Spaltenwechsel oder innerhalb Power zwischen Buttons
-- - ↑ in Power: zurück zur zuletzt genutzten Spalte
-- - Enter aktiviert, Esc schließt (über opts.handle)
---------------------------------------------------------------------
function Focus.attach_columns_power(left_items, right_items, power_items, th, opts)
	opts = opts or {}
	local keys = norm_keys(opts.keys)
	local mouse_follow = (opts.mouse_follow ~= false)

	local L, R, P = left_items or {}, right_items or {}, power_items or {}
	local nL, nR, nP = #L, #R, #P
	if (nL + nR + nP) == 0 then
		return function() end
	end

	-- Startzone bestimmen
	local zone = (opts.start_side == "right" and nR > 0) and "right" or "left"
	if zone == "left" and nL == 0 and nR > 0 then
		zone = "right"
	end
	if (zone ~= "left" and zone ~= "right") and nP > 0 then
		zone = "power"
	end

	local iL, iR, iP = 1, 1, 1
	local last_column = (zone == "right" and "right") or "left"

	local function set_focus_list(list, idx, on)
		local w = list[idx]
		if w and w.set_focus then
			pcall(w.set_focus, w, on, th)
		end
	end

	local function clear_all_focus()
		for i = 1, nL do
			pcall(set_focus_list, L, i, false)
		end
		for i = 1, nR do
			pcall(set_focus_list, R, i, false)
		end
		for i = 1, nP do
			pcall(set_focus_list, P, i, false)
		end
	end

	local function enter_zone(new_zone)
		if zone == new_zone then
			return
		end
		if zone == "left" and nL > 0 then
			set_focus_list(L, iL, false)
		end
		if zone == "right" and nR > 0 then
			set_focus_list(R, iR, false)
		end
		if zone == "power" and nP > 0 then
			set_focus_list(P, iP, false)
		end

		zone = new_zone
		if zone == "left" then
			last_column = "left"
		end
		if zone == "right" then
			last_column = "right"
		end

		if zone == "left" and nL > 0 then
			set_focus_list(L, iL, true)
		end
		if zone == "right" and nR > 0 then
			set_focus_list(R, iR, true)
		end
		if zone == "power" and nP > 0 then
			set_focus_list(P, iP, true)
		end
	end

	local function set_index(list, n, cur_idx, delta)
		local old = cur_idx
		cur_idx = math.max(1, math.min(n, cur_idx + delta))
		if cur_idx ~= old then
			set_focus_list(list, old, false)
			set_focus_list(list, cur_idx, true)
		end
		return cur_idx
	end

	-- initialen Fokus zeichnen
	if zone == "left" and nL > 0 then
		set_focus_list(L, iL, true)
	end
	if zone == "right" and nR > 0 then
		set_focus_list(R, iR, true)
	end
	if zone == "power" and nP > 0 then
		set_focus_list(P, iP, true)
	end

	-- Maus folgt Fokus (für alle drei Zonen)
	local enter_handlers = { L = {}, R = {}, P = {} }
	if mouse_follow then
		for idx, w in ipairs(L) do
			if w and w.connect_signal then
				local h = function()
					iL = idx
					enter_zone("left")
				end
				w:connect_signal("mouse::enter", h)
				enter_handlers.L[idx] = { obj = w, cb = h }
			end
		end
		for idx, w in ipairs(R) do
			if w and w.connect_signal then
				local h = function()
					iR = idx
					enter_zone("right")
				end
				w:connect_signal("mouse::enter", h)
				enter_handlers.R[idx] = { obj = w, cb = h }
			end
		end
		for idx, w in ipairs(P) do
			local tgt = (w and w.mouse_enter_target) or w
			if tgt and tgt.connect_signal then
				local h = function()
					iP = idx
					enter_zone("power")
				end
				tgt:connect_signal("mouse::enter", h)
				enter_handlers.P[idx] = { obj = tgt, cb = h }
			end
		end
	end

	-- Keygrabber
	local kg_id = awful.keygrabber.run(function(_, key, ev)
		if ev == "release" then
			return
		end

		if key == keys.left then
			if zone == "right" and nL > 0 then
				enter_zone("left")
			elseif zone == "power" and nP > 0 then
				iP = set_index(P, nP, iP, -1)
			end
		elseif key == keys.right then
			if zone == "left" and nR > 0 then
				enter_zone("right")
			elseif zone == "power" and nP > 0 then
				iP = set_index(P, nP, iP, 1)
			end
		elseif key == keys.up then
			if zone == "power" then
				if last_column == "right" and nR > 0 then
					enter_zone("right")
				elseif nL > 0 then
					enter_zone("left")
				end
			elseif zone == "left" and nL > 0 then
				iL = set_index(L, nL, iL, -1)
			elseif zone == "right" and nR > 0 then
				iR = set_index(R, nR, iR, -1)
			end
		elseif key == keys.down then
			if zone == "left" and nL > 0 then
				if iL < nL then
					iL = set_index(L, nL, iL, 1)
				else
					if nP > 0 then
						enter_zone("power")
					end
				end
			elseif zone == "right" and nR > 0 then
				if iR < nR then
					iR = set_index(R, nR, iR, 1)
				else
					if nP > 0 then
						enter_zone("power")
					end
				end
			elseif zone == "power" then
				-- kein wrap nach unten
			end
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
					local w
					if zone == "left" and nL > 0 then
						w = L[iL]
					elseif zone == "right" and nR > 0 then
						w = R[iR]
					elseif zone == "power" and nP > 0 then
						w = P[iP]
					end
					if w and w.activate then
						pcall(w.activate, w)
					end
					break
				end
			end
		end
	end)

	-- Cleanup
	return function()
		if kg_id then
			pcall(awful.keygrabber.stop, kg_id)
			kg_id = nil
		end
		for _, bucket in pairs(enter_handlers) do
			for idx, rec in ipairs(bucket) do
				if rec and rec.obj and rec.cb then
					pcall(rec.obj.disconnect_signal, rec.obj, "mouse::enter", rec.cb)
				end
				bucket[idx] = nil
			end
		end
		clear_all_focus()
	end
end

-- Dialog Fokus
function Focus.attach_dialog(body_items, cancel_item, th, opts)
	opts = opts or {}
	local keys = norm_keys(opts.keys)
	local items = {}
	for i = 1, #(body_items or {}) do
		items[i] = body_items[i]
	end
	if cancel_item then
		table.insert(items, cancel_item)
	end
	local n = #items
	if n == 0 then
		return function() end
	end

	local i = 1
	local enter_handlers = {}

	local function hi(idx, on)
		local w = items[idx]
		if w and w.set_focus then
			pcall(w.set_focus, w, on, th)
		end
	end
	local function set_index(new_i)
		if new_i == i then
			return
		end
		hi(i, false)
		i = math.max(1, math.min(n, new_i))
		hi(i, true)
	end

	-- initial
	hi(i, true)

	-- Maus folgt Fokus (optional, default an)
	if opts.mouse_follow ~= false then
		for idx, w in ipairs(items) do
			local tgt = (w and w.mouse_enter_target) or w
			if tgt and tgt.connect_signal then
				local h = function()
					set_index(idx)
				end
				tgt:connect_signal("mouse::enter", h)
				enter_handlers[idx] = { widget = tgt, handler = h }
			end
		end
	end

	-- Keygrabber: links/rechts im Body; ↓ => Cancel; ↑ von Cancel zurück
	local kg_id = awful.keygrabber.run(function(_, key, ev)
		if ev == "release" then
			return
		end

		if key == keys.left then
			if i < n then
				set_index(i - 1)
			end
		elseif key == keys.right then
			if i < n then
				set_index(i + 1)
			end
		elseif key == keys.down then
			set_index(n) -- immer Cancel
		elseif key == keys.up then
			if i == n then
				set_index(math.max(1, n - 1))
			else
				set_index(math.max(1, i - 1))
			end
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
		end
		for _, e in pairs(enter_handlers) do
			pcall(e.widget.disconnect_signal, e.widget, "mouse::enter", e.handler)
		end
		for idx = 1, n do
			pcall(hi, idx, false)
		end
	end
end

return Focus
