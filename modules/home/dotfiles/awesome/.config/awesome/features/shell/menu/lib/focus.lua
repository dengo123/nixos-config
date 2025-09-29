-- ~/.config/awesome/features/shell/menu/lib/focus.lua
local awful = require("awful")

local Focus = {}

-- ---------- helpers ----------

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

local function call_set_focus(w, on, th)
	if w and w.set_focus then
		pcall(w.set_focus, w, on, th)
	end
end
local function call_activate(w)
	if w and w.activate then
		pcall(w.activate, w)
	end
end

local function ok_hit(keys, key)
	local oks = (type(keys.ok) == "table") and keys.ok or { keys.ok }
	for _, k in ipairs(oks) do
		if key == k then
			return true
		end
	end
	return false
end

-- mouse follow for a list (array) of focusable widgets
local function install_mouse_follow(list, set_index)
	local handlers = {}
	for idx, w in ipairs(list or {}) do
		local tgt = (w and w.mouse_enter_target) or w
		if tgt and tgt.connect_signal then
			local h = function()
				set_index(idx)
			end
			tgt:connect_signal("mouse::enter", h)
			handlers[#handlers + 1] = { obj = tgt, cb = h }
		end
	end
	return function()
		for _, rec in ipairs(handlers) do
			if rec.obj and rec.cb then
				pcall(rec.obj.disconnect_signal, rec.obj, "mouse::enter", rec.cb)
			end
		end
	end
end

-- one keygrabber to rule them all
local function run_keygrabber(on_key)
	local kg_id = awful.keygrabber.run(function(_, key, ev)
		if ev ~= "release" then
			on_key(key)
		end
	end)
	return function()
		if kg_id then
			pcall(awful.keygrabber.stop, kg_id)
		end
	end
end

local function close_handle(handle)
	if handle and handle.close then
		pcall(function()
			handle:close()
		end)
	elseif handle and handle.hide then
		pcall(function()
			handle:hide()
		end)
	end
end

-- ---------- 1) linear list ----------
function Focus.attach(items, th, opts)
	opts = opts or {}
	local keys = norm_keys(opts.keys)
	local n = #items
	if n == 0 then
		return function() end
	end

	local i = 1
	local function set_index(new_i)
		new_i = math.max(1, math.min(n, new_i))
		if new_i == i then
			return
		end
		call_set_focus(items[i], false, th)
		i = new_i
		call_set_focus(items[i], true, th)
	end

	-- initial focus
	call_set_focus(items[i], true, th)

	local remove_mouse = nil
	if opts.mouse_follow ~= false then
		remove_mouse = install_mouse_follow(items, set_index)
	else
		remove_mouse = function() end
	end

	local stop_kg = run_keygrabber(function(key)
		if key == keys.left or key == keys.up then
			set_index(i - 1)
		elseif key == keys.right or key == keys.down then
			set_index(i + 1)
		elseif key == keys.cancel then
			close_handle(opts.handle)
		elseif ok_hit(keys, key) then
			call_activate(items[i])
		end
	end)

	return function()
		stop_kg()
		remove_mouse()
		for idx = 1, n do
			call_set_focus(items[idx], false, th)
		end
	end
end

-- ---------- 2) two columns + optional power row ----------
function Focus.attach_columns_power(left_items, right_items, power_items, th, opts)
	opts = opts or {}
	local keys = norm_keys(opts.keys)

	local L, R, P = left_items or {}, right_items or {}, power_items or {}
	local nL, nR, nP = #L, #R, #P
	if (nL + nR + nP) == 0 then
		return function() end
	end

	local zone = (opts.start_side == "right" and nR > 0) and "right" or "left"
	if zone == "left" and nL == 0 and nR > 0 then
		zone = "right"
	end
	if (zone ~= "left" and zone ~= "right") and nP > 0 then
		zone = "power"
	end
	local last_col = (zone == "right") and "right" or "left"

	local iL, iR, iP = 1, 1, 1

	local function set_focus(list, idx, on)
		call_set_focus(list[idx], on, th)
	end

	local function enter_zone(new_zone)
		if zone == new_zone then
			return
		end
		if zone == "left" and nL > 0 then
			set_focus(L, iL, false)
		end
		if zone == "right" and nR > 0 then
			set_focus(R, iR, false)
		end
		if zone == "power" and nP > 0 then
			set_focus(P, iP, false)
		end

		zone = new_zone
		if zone == "left" then
			last_col = "left"
		end
		if zone == "right" then
			last_col = "right"
		end

		if zone == "left" and nL > 0 then
			set_focus(L, iL, true)
		end
		if zone == "right" and nR > 0 then
			set_focus(R, iR, true)
		end
		if zone == "power" and nP > 0 then
			set_focus(P, iP, true)
		end
	end

	local function step(list, n, idx, delta)
		local old = idx
		idx = math.max(1, math.min(n, idx + delta))
		if idx ~= old then
			set_focus(list, old, false)
			set_focus(list, idx, true)
		end
		return idx
	end

	-- initial focus
	if zone == "left" and nL > 0 then
		set_focus(L, iL, true)
	end
	if zone == "right" and nR > 0 then
		set_focus(R, iR, true)
	end
	if zone == "power" and nP > 0 then
		set_focus(P, iP, true)
	end

	local remove_mouse = function() end
	if opts.mouse_follow ~= false then
		local rmL = install_mouse_follow(L, function(idx)
			iL = idx
			enter_zone("left")
		end)
		local rmR = install_mouse_follow(R, function(idx)
			iR = idx
			enter_zone("right")
		end)
		local rmP = install_mouse_follow(P, function(idx)
			iP = idx
			enter_zone("power")
		end)
		remove_mouse = function()
			rmL()
			rmR()
			rmP()
		end
	end

	local stop_kg = run_keygrabber(function(key)
		if key == keys.left then
			if zone == "right" and nL > 0 then
				enter_zone("left")
			elseif zone == "power" and nP > 0 then
				iP = step(P, nP, iP, -1)
			end
		elseif key == keys.right then
			if zone == "left" and nR > 0 then
				enter_zone("right")
			elseif zone == "power" and nP > 0 then
				iP = step(P, nP, iP, 1)
			end
		elseif key == keys.up then
			if zone == "power" then
				if last_col == "right" and nR > 0 then
					enter_zone("right")
				elseif nL > 0 then
					enter_zone("left")
				end
			elseif zone == "left" and nL > 0 then
				iL = step(L, nL, iL, -1)
			elseif zone == "right" and nR > 0 then
				iR = step(R, nR, iR, -1)
			end
		elseif key == keys.down then
			if zone == "left" and nL > 0 then
				if iL < nL then
					iL = step(L, nL, iL, 1)
				elseif nP > 0 then
					enter_zone("power")
				end
			elseif zone == "right" and nR > 0 then
				if iR < nR then
					iR = step(R, nR, iR, 1)
				elseif nP > 0 then
					enter_zone("power")
				end
			end
		elseif key == keys.cancel then
			close_handle(opts.handle)
		elseif ok_hit(keys, key) then
			local w = (zone == "left" and L[iL]) or (zone == "right" and R[iR]) or (zone == "power" and P[iP])
			call_activate(w)
		end
	end)

	return function()
		stop_kg()
		remove_mouse()
		for x = 1, nL do
			call_set_focus(L[x], false, th)
		end
		for x = 1, nR do
			call_set_focus(R[x], false, th)
		end
		for x = 1, nP do
			call_set_focus(P[x], false, th)
		end
	end
end

-- ---------- 3) dialog (body list + optional cancel) ----------
function Focus.attach_dialog(body_items, cancel_item, th, opts)
	opts = opts or {}
	local keys = norm_keys(opts.keys)

	local list = {}
	for i = 1, #(body_items or {}) do
		list[i] = body_items[i]
	end
	if cancel_item then
		list[#list + 1] = cancel_item
	end
	local n = #list
	if n == 0 then
		return function() end
	end

	local i = 1
	local last_body = math.max(1, n - (cancel_item and 1 or 0))

	local function set_index(new_i)
		new_i = math.max(1, math.min(n, new_i))
		if new_i == i then
			return
		end
		call_set_focus(list[i], false, th)
		i = new_i
		call_set_focus(list[i], true, th)
	end

	call_set_focus(list[i], true, th)

	local remove_mouse = function() end
	if opts.mouse_follow ~= false then
		remove_mouse = install_mouse_follow(list, set_index)
	end

	local stop_kg = run_keygrabber(function(key)
		if key == keys.left then
			set_index(i - 1)
		elseif key == keys.right then
			set_index(i + 1)
		elseif key == keys.up then
			if i == n and cancel_item then
				set_index(last_body)
			else
				set_index(i - 1)
			end
		elseif key == keys.down then
			if cancel_item then
				set_index(n)
			else
				set_index(i + 1)
			end
		elseif key == keys.cancel then
			close_handle(opts.handle)
		elseif ok_hit(keys, key) then
			call_activate(list[i])
		end
	end)

	return function()
		stop_kg()
		remove_mouse()
		for idx = 1, n do
			call_set_focus(list[idx], false, th)
		end
	end
end

return Focus
