-- ~/.config/awesome/features/shell/menu/lib/focus.lua
local awful = require("awful")

local Focus = {}

local function run_keygrabber(handler)
	-- Funktionale API für Awesome 4.3+ (kompatibel)
	local id = awful.keygrabber.run(handler)
	return function()
		if id then
			pcall(awful.keygrabber.stop, id)
		end
	end
end

local function mk_setter(items, th)
	return function(idx, on)
		local w = items[idx]
		if w and w.set_focus then
			w:set_focus(on, th)
		end
	end
end

-- 1) HORIZONTAL: ←/→ (+Tab/Shift+Tab), Enter
function Focus.attach(items, th, opts)
	if not items or #items == 0 then
		return function() end
	end
	local n, i = #items, 1
	local set = mk_setter(items, th)

	local function move(delta)
		set(i, false)
		i = ((i - 1 + delta) % n) + 1
		set(i, true)
	end

	set(1, true)
	local stop = run_keygrabber(function(_, key, ev)
		if ev == "release" then
			return
		end
		if key == "Right" or key == "Tab" then
			move(1)
		elseif key == "Left" or key == "ISO_Left_Tab" then
			move(-1)
		elseif key == "Return" or key == "KP_Enter" then
			local w = items[i]
			if w and w.activate then
				w:activate()
			end
		elseif key == "Escape" then
			local h = opts and opts.handle
			if h and h.close then
				h.close()
			elseif h and h.hide then
				h:hide()
			end
		end
	end)

	return function()
		pcall(stop)
		for j = 1, n do
			set(j, false)
		end
	end
end

-- 2) VERTIKAL: ↑/↓ (+Home/End), Enter
function Focus.attach_vertical(items, th, opts)
	if not items or #items == 0 then
		return function() end
	end
	local n, i = #items, 1
	local set = mk_setter(items, th)

	local function to(idx)
		set(i, false)
		i = idx
		set(i, true)
	end
	set(1, true)

	local stop = run_keygrabber(function(_, key, ev)
		if ev == "release" then
			return
		end
		if key == "Down" then
			to(math.min(n, i + 1))
		elseif key == "Up" then
			to(math.max(1, i - 1))
		elseif key == "Home" then
			to(1)
		elseif key == "End" then
			to(n)
		elseif key == "Return" or key == "KP_Enter" then
			local w = items[i]
			if w and w.activate then
				w:activate()
			end
		elseif key == "Escape" then
			local h = opts and opts.handle
			if h and h.close then
				h.close()
			elseif h and h.hide then
				h:hide()
			end
		end
	end)

	return function()
		pcall(stop)
		for j = 1, n do
			set(j, false)
		end
	end
end

-- 3) SPALTEN: links/rechts + ↑/↓, Enter, Esc
-- cols = { left = {widgets...}, right = {widgets...} }
function Focus.attach_columns(cols, th, opts)
	cols = cols or {}
	local left = cols.left or {}
	local right = cols.right or {}
	local lists = { left = left, right = right }
	local side = (opts and opts.start_side) or "left"
	if #lists[side] == 0 then
		side = (#right > 0) and "right" or "left"
	end
	local i = 1

	local function set(on)
		local w = lists[side][i]
		if w and w.set_focus then
			w:set_focus(on, th)
		end
	end
	local function clamp()
		local n = #lists[side]
		if n == 0 then
			i = 1
		else
			i = math.max(1, math.min(i, n))
		end
	end

	clamp()
	set(true)

	local stop = run_keygrabber(function(_, key, ev)
		if ev == "release" then
			return
		end
		local n = #lists[side]
		if key == "Down" then
			if n > 0 then
				set(false)
				i = math.min(n, i + 1)
				set(true)
			end
		elseif key == "Up" then
			if n > 0 then
				set(false)
				i = math.max(1, i - 1)
				set(true)
			end
		elseif key == "Right" or key == "Tab" then
			set(false)
			side = (side == "left") and "right" or "left"
			clamp()
			set(true)
		elseif key == "Left" or key == "ISO_Left_Tab" then
			set(false)
			side = (side == "right") and "left" or "right"
			clamp()
			set(true)
		elseif key == "Return" or key == "KP_Enter" then
			local w = lists[side][i]
			if w and w.activate then
				w:activate()
			end
		elseif key == "Escape" then
			local h = opts and opts.handle
			if h and h.close then
				h.close()
			elseif h and h.hide then
				h:hide()
			end
		end
	end)

	return function()
		pcall(stop)
		for _, lst in pairs(lists) do
			for _, w in ipairs(lst) do
				if w.set_focus then
					w:set_focus(false, th)
				end
			end
		end
	end
end

return Focus
