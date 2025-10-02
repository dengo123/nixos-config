-- ~/.config/awesome/shell/menu/lib/focus.lua
local awful = require("awful")

local Focus = {}

-- normalize key map
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

-- PUBLIC: linear focus list
-- items: { w1, w2, ... } each supporting set_focus(on,th) and activate()
-- opts:  { keys?, mouse_follow?, handle? }
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

	-- object-based keygrabber (robust)
	local kg = awful.keygrabber({
		autostart = true,
		stop_event = "release",
		keypressed_callback = function(_, key)
			if key == keys.left or key == keys.up then
				set_index(i - 1)
			elseif key == keys.right or key == keys.down then
				set_index(i + 1)
			elseif key == keys.cancel then
				if opts.handle and opts.handle.close then
					-- stop first, then close to avoid any race
					pcall(function()
						kg:stop()
					end)
					pcall(function()
						remove_mouse()
					end)
					call_set_focus(items[i], false, th)
					return opts.handle.close()
				end
			elseif ok_hit(keys, key) then
				call_activate(items[i])
			end
		end,
	})

	-- stop function returned to caller
	return function()
		pcall(function()
			kg:stop()
		end)
		pcall(function()
			remove_mouse()
		end)
		for idx = 1, n do
			call_set_focus(items[idx], false, th)
		end
	end
end

return Focus
