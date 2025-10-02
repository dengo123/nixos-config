-- ~/.config/awesome/shell/menu/lib/focus.lua
local awful = require("awful")

local Focus = {}

-- ---------------- helpers ----------------

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

local function install_mouse_follow(list, set_index)
	local handlers = {}
	for idx, w in ipairs(list or {}) do
		local tgt = (w and w.mouse_enter_target) or w
		if tgt and tgt.connect_signal then
			local cb = function()
				set_index(idx)
			end
			tgt:connect_signal("mouse::enter", cb)
			handlers[#handlers + 1] = { obj = tgt, cb = cb }
		end
	end
	return function()
		for _, h in ipairs(handlers) do
			if h.obj and h.cb then
				pcall(h.obj.disconnect_signal, h.obj, "mouse::enter", h.cb)
			end
		end
	end
end

-- Singleton-Keygrabber, verhindert Doppel-Grab
local current_kg = nil

local function make_keygrabber(bindings)
	-- alten Grabber sicher beenden
	if current_kg then
		pcall(function()
			current_kg:stop()
		end)
		current_kg = nil
	end

	local kg = awful.keygrabber({
		autostart = true,
		-- stop_event = "release",  -- absichtlich NICHT gesetzt
		keybindings = bindings,
	})

	current_kg = kg

	-- Rückgabe: Stopp-Funktion für genau diesen Grabber
	return function()
		if kg then
			pcall(function()
				kg:stop()
			end)
		end
		if current_kg == kg then
			current_kg = nil
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

-- --------------- public API ----------------
-- items: {widget1, widget2, ...} (Widgets mit set_focus/activate empfohlen)
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

	-- Maus-Follow (optional)
	local remove_mouse = (opts.mouse_follow == false) and function() end or install_mouse_follow(items, set_index)

	-- Wir wollen den Grabber auch bei ESC sofort stoppen:
	local stop_kg_ref = function() end

	local bindings = {
		{
			{},
			keys.left,
			function()
				set_index(i - 1)
			end,
		},
		{
			{},
			keys.up,
			function()
				set_index(i - 1)
			end,
		},
		{
			{},
			keys.right,
			function()
				set_index(i + 1)
			end,
		},
		{
			{},
			keys.down,
			function()
				set_index(i + 1)
			end,
		},
		{
			{},
			keys.cancel,
			function()
				-- Grabber sofort stoppen, dann Handle schließen
				stop_kg_ref()
				close_handle(opts.handle)
			end,
		},
	}

	-- OK-Keys (Return, KP_Enter …)
	local oks = (type(keys.ok) == "table") and keys.ok or { keys.ok }
	for _, k in ipairs(oks) do
		table.insert(bindings, {
			{},
			k,
			function()
				call_activate(items[i])
			end,
		})
	end

	local stop_kg = make_keygrabber(bindings)
	stop_kg_ref = stop_kg

	-- Cleanup-Funktion für den Aufrufer
	return function()
		stop_kg()
		remove_mouse()
		for idx = 1, n do
			call_set_focus(items[idx], false, th)
		end
	end
end

return Focus
