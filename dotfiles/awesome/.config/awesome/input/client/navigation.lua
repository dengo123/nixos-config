-- ~/.config/awesome/input/client/navigation.lua
local awful = require("awful")

local M = {}

-- =========================================================================
-- Helpers
-- =========================================================================

local function kbd_intent(ms)
	awesome.emit_signal("focus_policy::keyboard_intent", ms or 250)
end

local function is_max_layout(s)
	s = s or awful.screen.focused()
	return awful.layout.get(s) == awful.layout.suit.max
end

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

	local target_idx = ((idx - 1 + (dir or 1)) % #list) + 1
	local target = list[target_idx]

	if target and target.valid then
		if target.minimized then
			target.minimized = false
		end

		kbd_intent()
		target:emit_signal("request::activate", "keynav", { raise = true })
	end
end

local function cycle_max(dir)
	local s = (client.focus and client.focus.valid) and client.focus.screen or awful.screen.focused()
	local list = clients_of_selected_tag(s, true)
	focus_in_list(list, dir)
end

local function focus_client(dir)
	if is_max_layout() then
		if dir == "right" then
			cycle_max(1)
		elseif dir == "left" then
			cycle_max(-1)
		end
		return
	end

	kbd_intent()
	awful.client.focus.bydirection(dir)
end

local function swap_client(dir)
	if is_max_layout() then
		return
	end

	kbd_intent()
	awful.client.swap.bydirection(dir)
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.build(modkey)
	return awful.util.table.join(
		awful.key({ modkey }, "Right", function()
			focus_client("right")
		end, {
			description = "Focus Client Right",
			group = "Client",
		}),

		awful.key({ modkey }, "Left", function()
			focus_client("left")
		end, {
			description = "Focus Client Left",
			group = "Client",
		}),

		awful.key({ modkey }, "Up", function()
			if not is_max_layout() then
				focus_client("up")
			end
		end, {
			description = "Focus Client Up",
			group = "Client",
		}),

		awful.key({ modkey }, "Down", function()
			if not is_max_layout() then
				focus_client("down")
			end
		end, {
			description = "Focus Client Down",
			group = "Client",
		}),

		awful.key({ modkey, "Shift" }, "Right", function()
			swap_client("right")
		end, {
			description = "Move Window Right",
			group = "Client",
		}),

		awful.key({ modkey, "Shift" }, "Left", function()
			swap_client("left")
		end, {
			description = "Move Window Left",
			group = "Client",
		}),

		awful.key({ modkey, "Shift" }, "Up", function()
			swap_client("up")
		end, {
			description = "Move Window Up",
			group = "Client",
		}),

		awful.key({ modkey, "Shift" }, "Down", function()
			swap_client("down")
		end, {
			description = "Move Window Down",
			group = "Client",
		})
	)
end

return function(modkey)
	return M.build(modkey)
end
