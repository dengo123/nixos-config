-- ~/.config/awesome/features/shell/menu/lib/focus.lua
local awful = require("awful")

local Focus = {}

-- --- helpers ---------------------------------------------------------

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

-- --- einzig öffentliche API: lineare Liste --------------------------
-- items: {widget1, widget2, ...} (Widgets sollten set_focus/activate unterstützen)
-- opts: { keys?, mouse_follow?, handle? }
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

	local remove_mouse = (opts.mouse_follow == false) and function() end or install_mouse_follow(items, set_index)

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

return Focus
