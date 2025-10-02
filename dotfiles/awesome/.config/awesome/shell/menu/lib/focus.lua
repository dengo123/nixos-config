-- ~/.config/awesome/shell/menu/lib/focus.lua
-- Fokussierung ohne keygrabber: temporäre, modale root.keys()-Bindings
local awful = require("awful")
local gears = require("gears")

local Focus = {}

-- ------------ helpers ------------
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

-- Alle Mod-Kombinationen, damit „Mod festhalten“ nicht blockiert
local MODS = {
	{},
	{ "Mod4" },
	{ "Shift" },
	{ "Control" },
	{ "Mod1" },
	{ "Mod4", "Shift" },
	{ "Mod4", "Control" },
	{ "Mod4", "Mod1" },
	{ "Shift", "Control" },
	{ "Shift", "Mod1" },
	{ "Control", "Mod1" },
	{ "Mod4", "Shift", "Control" },
	{ "Mod4", "Shift", "Mod1" },
	{ "Mod4", "Control", "Mod1" },
	{ "Shift", "Control", "Mod1" },
	{ "Mod4", "Shift", "Control", "Mod1" },
}

local function add_key(all, mods, key, fn, desc)
	table.insert(all, awful.key(mods, key, fn, { description = desc or key, group = "focus" }))
end

-- ------------ public API ------------
-- items: { widget1, widget2, ... } (Widgets mit :set_focus/:activate empfohlen)
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

	-- 1) Alte global keys sichern
	local old_global = root.keys() -- Achtung: kann nil sein, abfangen
	local joined = old_global

	-- 2) Temporäre Focus-Keymap bauen (für alle Mod-Kombinationen)
	local temp = {}

	for _, mods in ipairs(MODS) do
		add_key(temp, mods, keys.left, function()
			set_index(i - 1)
		end, "focus-left")
		add_key(temp, mods, keys.up, function()
			set_index(i - 1)
		end, "focus-up")
		add_key(temp, mods, keys.right, function()
			set_index(i + 1)
		end, "focus-right")
		add_key(temp, mods, keys.down, function()
			set_index(i + 1)
		end, "focus-down")
		-- ESC schließt Dialog
		add_key(temp, mods, keys.cancel, function()
			close_handle(opts.handle)
		end, "focus-cancel")
	end

	local oks = (type(keys.ok) == "table") and keys.ok or { keys.ok }
	for _, k in ipairs(oks) do
		for _, mods in ipairs(MODS) do
			add_key(temp, mods, k, function()
				call_activate(items[i])
			end, "focus-activate")
		end
	end

	-- 3) Aktivieren (anhängen, nicht ersetzen)
	if joined then
		joined = gears.table.join(joined, table.unpack(temp))
	else
		joined = gears.table.join(table.unpack(temp))
	end
	root.keys(joined)

	-- Cleanup: alte Keys wiederherstellen, Maus-Follow entfernen, Fokus resetten
	return function()
		-- Fokus-Reset
		for idx = 1, n do
			call_set_focus(items[idx], false, th)
		end
		remove_mouse()
		-- alte global keys zurück
		if old_global then
			root.keys(old_global)
		else
			-- falls vorher nil: alle globalen Keys entfernen
			root.keys(nil)
		end
	end
end

return Focus
