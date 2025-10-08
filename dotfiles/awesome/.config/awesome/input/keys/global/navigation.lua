-- ~/.config/awesome/input/keys/global/navigation.lua
local awful = require("awful")

-- Nur wenn eine Tastatur-Interaktion stattfand, darf zentriert werden:
local function kbd_intent(ms)
	awesome.emit_signal("focus_policy::keyboard_intent", ms or 250)
end

local function is_max_layout(s)
	s = s or awful.screen.focused()
	return awful.layout.get(s) == awful.layout.suit.max
end

-- Alle Clients des ausgewählten Tags in stabiler Tag-Reihenfolge
local function clients_of_selected_tag(s, include_minimized)
	s = s or awful.screen.focused()
	local t = s.selected_tag
	if not t then
		return {}
	end
	local out = {}
	for _, c in ipairs(t:clients() or {}) do
		if c.valid and not c.skip_taskbar and c.screen == s then
			if include_minimized or not c.minimized then
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
	local cur, idx = client.focus, 0
	if cur then
		for i, c in ipairs(list) do
			if c == cur then
				idx = i
				break
			end
		end
	end
	local j = ((idx - 1 + (dir or 1)) % #list) + 1
	local target = list[j]
	if target and target.valid then
		if target.minimized then
			target.minimized = false
		end
		kbd_intent()
		target:emit_signal("request::activate", "keynav", { raise = true })
	end
end

-- Max: stumpf über alle Clients des aktiven Tags cyclen
local function cycle_max(dir)
	local s = (client.focus and client.focus.valid) and client.focus.screen or awful.screen.focused()
	local list = clients_of_selected_tag(s, true) -- include minimized
	focus_in_list(list, dir)
end

return function(modkey)
	return awful.util.table.join(
		-- Fokus bewegen
		awful.key({ modkey }, "Right", function()
			if is_max_layout() then
				cycle_max(1)
			else
				kbd_intent()
				awful.client.focus.bydirection("right")
			end
		end, { description = "focus next (max: cycle all / tiled: →)", group = "client" }),
		awful.key({ modkey }, "Left", function()
			if is_max_layout() then
				cycle_max(-1)
			else
				kbd_intent()
				awful.client.focus.bydirection("left")
			end
		end, { description = "focus prev (max: cycle all / tiled: ←)", group = "client" }),
		awful.key({ modkey }, "Up", function()
			if not is_max_layout() then
				kbd_intent()
				awful.client.focus.bydirection("up")
			end
		end, { description = "focus up (tiled)", group = "client" }),

		awful.key({ modkey }, "Down", function()
			if not is_max_layout() then
				kbd_intent()
				awful.client.focus.bydirection("down")
			end
		end, { description = "focus down (tiled)", group = "client" }),

		-- Fenster verschieben (nur tiled; in max absichtlich nichts)
		awful.key({ modkey, "Shift" }, "Right", function()
			if not is_max_layout() then
				kbd_intent()
				awful.client.swap.bydirection("right")
			end
		end, { description = "move window right (tiled)", group = "client" }),
		awful.key({ modkey, "Shift" }, "Left", function()
			if not is_max_layout() then
				kbd_intent()
				awful.client.swap.bydirection("left")
			end
		end, { description = "move window left (tiled)", group = "client" }),

		awful.key({ modkey, "Shift" }, "Up", function()
			if not is_max_layout() then
				kbd_intent()
				awful.client.swap.bydirection("up")
			end
		end, { description = "move window up (tiled)", group = "client" }),

		awful.key({ modkey, "Shift" }, "Down", function()
			if not is_max_layout() then
				kbd_intent()
				awful.client.swap.bydirection("down")
			end
		end, { description = "move window down (tiled)", group = "client" })
	)
end
