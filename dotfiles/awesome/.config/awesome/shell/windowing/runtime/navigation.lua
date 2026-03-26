-- ~/.config/awesome/shell/windowing/behavior/navigation.lua
local awful = require("awful")

local M = {}

local runtime = {
	api = {},
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function minimized_api()
	return runtime.api.minimized
end

local function current_nav_screen()
	local focused = awful.screen.focused()
	if focused and focused.valid then
		return focused
	end

	if client.focus and client.focus.valid and not client.focus.minimized and client.focus.screen then
		return client.focus.screen
	end

	return awful.screen.primary
end

local function kbd_intent(ms)
	awesome.emit_signal("focus_policy::keyboard_intent", ms or 250)
end

local function is_max_layout(s)
	s = s or current_nav_screen()
	return awful.layout.get(s) == awful.layout.suit.max
end

local function selected_tag_clients(s, include_minimized)
	s = s or current_nav_screen()

	local t = s and s.selected_tag or nil
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

local function activate_target(target)
	if not (target and target.valid) then
		return
	end

	if target.screen and target.screen.valid then
		awful.screen.focus(target.screen)
	end

	if target.minimized then
		target.minimized = false
	end

	local Minimized = minimized_api()
	if Minimized and type(Minimized.remove) == "function" then
		Minimized.remove(target)
	end

	kbd_intent()
	target:emit_signal("request::activate", "keynav", { raise = true })
end

local function focus_in_list(list, dir)
	if #list == 0 then
		return false
	end

	local cur = client.focus
	local idx = 0

	if cur and cur.valid and not cur.minimized then
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
		activate_target(target)
		return true
	end

	return false
end

local function restore_minimized_on_screen(s)
	local Minimized = minimized_api()
	if not (Minimized and type(Minimized.pop_on_screen) == "function") then
		return false
	end

	local target = Minimized.pop_on_screen(s)
	if not target then
		return false
	end

	activate_target(target)
	return true
end

local function cycle_max(dir)
	local s = current_nav_screen()
	local list = selected_tag_clients(s, true)

	if focus_in_list(list, dir) then
		return
	end

	restore_minimized_on_screen(s)
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(args)
	args = args or {}
	runtime.api = args.api or {}
	return M
end

function M.focus_client(dir)
	local s = current_nav_screen()

	if is_max_layout(s) then
		if dir == "right" or dir == "down" then
			cycle_max(1)
		elseif dir == "left" or dir == "up" then
			cycle_max(-1)
		end
		return
	end

	kbd_intent()
	awful.client.focus.bydirection(dir)
end

function M.swap_client(dir)
	local s = current_nav_screen()

	if is_max_layout(s) then
		return
	end

	kbd_intent()
	awful.client.swap.bydirection(dir)
end

function M.current_screen()
	return current_nav_screen()
end

function M.is_max_layout(s)
	return is_max_layout(s)
end

return M
