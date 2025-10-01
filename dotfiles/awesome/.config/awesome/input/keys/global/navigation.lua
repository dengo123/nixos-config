-- ~/.config/awesome/input/keys/global/navigation.lua
local awful = require("awful")
local H = require("input.keys.helpers")

local function is_max_layout(s)
	s = s or awful.screen.focused()
	return awful.layout.get(s) == awful.layout.suit.max
end

-- Clients des aktiven Tags sammeln (sichtbar, nicht minimized)
local function clients_on_tag(s, predicate)
	s = s or awful.screen.focused()
	local t = s.selected_tag
	if not t then
		return {}
	end
	local out = {}
	for _, c in ipairs(s.clients) do
		if c.valid and not c.minimized then
			local on_tag = false
			for _, tc in ipairs(c:tags()) do
				if tc == t then
					on_tag = true
					break
				end
			end
			if on_tag and (not predicate or predicate(c)) then
				table.insert(out, c)
			end
		end
	end
	return out
end

local function focus_in_list(list, dir)
	if #list == 0 then
		return
	end
	local cur = client.focus
	local idx = 0
	if cur then
		for i, c in ipairs(list) do
			if c == cur then
				idx = i
				break
			end
		end
	end
	local n = #list
	local j = ((idx - 1 + (dir or 1)) % n) + 1
	local target = list[j]
	if target and target.valid then
		target:emit_signal("request::activate", "keynav", { raise = true })
	end
end

local function resolve_screen()
	if client.focus and client.focus.valid then
		return client.focus.screen
	end
	if mouse and mouse.screen then
		return mouse.screen
	end
	return awful.screen.focused()
end

local function cycle_all(dir)
	local s = resolve_screen()
	local list = clients_on_tag(s, function(c)
		return awful.client.focus.filter(c)
	end)
	focus_in_list(list, dir)
end

local function cycle_same_class(dir)
	local cur = client.focus
	if not (cur and cur.valid and cur.class) then
		cycle_all(dir)
		return
	end
	local s = cur.screen or resolve_screen()
	local cls = cur.class
	local list = clients_on_tag(s, function(c)
		return awful.client.focus.filter(c) and c.class == cls
	end)
	if #list <= 1 then
		return
	end
	focus_in_list(list, dir)
end

return function(modkey)
	return awful.util.table.join(
		-- Fokus bewegen: in max ⇒ zyklisch durch alle/tabs, sonst bydirection
		awful.key({ modkey }, "Right", function()
			if is_max_layout() then
				cycle_all(1)
			else
				awful.client.focus.bydirection("right")
			end
		end, { description = "focus next (tabs in max)", group = "client" }),

		awful.key({ modkey }, "Left", function()
			if is_max_layout() then
				cycle_all(-1)
			else
				awful.client.focus.bydirection("left")
			end
		end, { description = "focus previous (tabs in max)", group = "client" }),

		-- Bonus: in max innerhalb gleicher Klasse
		awful.key({ modkey }, "Up", function()
			if is_max_layout() then
				cycle_same_class(1)
			else
				awful.client.focus.bydirection("up")
			end
		end, { description = "focus next of same class (max)", group = "client" }),

		awful.key({ modkey }, "Down", function()
			if is_max_layout() then
				cycle_same_class(-1)
			else
				awful.client.focus.bydirection("down")
			end
		end, { description = "focus prev of same class (max)", group = "client" }),

		-- Fenster bewegen (robust ohne boolesches Kurzschluss-Konstrukt)
		awful.key({ modkey, "Shift" }, "Left", function()
			if H and H.move_client_dir then
				H.move_client_dir("left")
			end
		end, { description = "move window left", group = "client" }),

		awful.key({ modkey, "Shift" }, "Right", function()
			if H and H.move_client_dir then
				H.move_client_dir("right")
			end
		end, { description = "move window right", group = "client" }),

		awful.key({ modkey, "Shift" }, "Up", function()
			if H and H.move_client_dir then
				H.move_client_dir("up")
			end
		end, { description = "move window up", group = "client" }),

		awful.key({ modkey, "Shift" }, "Down", function()
			if H and H.move_client_dir then
				H.move_client_dir("down")
			end
		end, { description = "move window down", group = "client" })
	)
end
