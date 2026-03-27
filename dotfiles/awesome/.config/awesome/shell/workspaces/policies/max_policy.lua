-- ~/.config/awesome/shell/workspaces/policies/max_policy.lua
local awful = require("awful")
local gears = require("gears")

local M = {}

local runtime = {
	ctx = {},
	max_solo = false,
	solo = nil,
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function ctx()
	return runtime.ctx or {}
end

local function cfg()
	return ctx().cfg or {}
end

local function solo_api()
	return runtime.solo
end

local function minimized_api()
	local c = ctx()

	return (c.services and c.services.minimized)
		or (c.external and c.external.minimized)
		or (c.api and c.api.minimized)
		or nil
end

local function current_nav_screen()
	local focused = awful.screen.focused()
	if focused and focused.valid then
		return focused
	end

	if client.focus and client.focus.valid and not client.focus.minimized and client.focus.screen then
		return client.focus.screen
	end

	return screen.primary
end

local function current_tag(s)
	s = s or current_nav_screen()
	return s and s.selected_tag or nil
end

local function dir_step(dir)
	if dir == "right" or dir == "down" then
		return 1
	end

	if dir == "left" or dir == "up" then
		return -1
	end

	return 1
end

local function kbd_intent(ms)
	awesome.emit_signal("focus_policy::keyboard_intent", ms or 250)
end

local function is_max_layout(s)
	s = s or current_nav_screen()
	return s and awful.layout.get(s) == awful.layout.suit.max
end

local function tag_clients(t, include_minimized)
	if not t then
		return {}
	end

	local s = t.screen
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

local function selected_tag_clients(s, include_minimized)
	return tag_clients(current_tag(s or current_nav_screen()), include_minimized)
end

local function remove_from_minimized(c)
	local Minimized = minimized_api()
	if Minimized and type(Minimized.remove) == "function" then
		Minimized.remove(c)
	end
end

local function activate_target(target)
	if not (target and target.valid) then
		return false
	end

	if target.screen and target.screen.valid then
		awful.screen.focus(target.screen)
	end

	if target.minimized then
		target.minimized = false
	end

	remove_from_minimized(target)

	kbd_intent()
	target:emit_signal("request::activate", "keynav", { raise = true })

	return true
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

	local step = dir_step(dir)
	local target_idx = ((idx - 1 + step) % #list) + 1
	local target = list[target_idx]

	return activate_target(target)
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

	return activate_target(target)
end

local function cycle_mode(dir, s)
	local list = selected_tag_clients(s, true)

	if focus_in_list(list, dir) then
		return true
	end

	return restore_minimized_on_screen(s)
end

local function solo_mode(dir, s)
	local t = current_tag(s)
	local list = tag_clients(t, true)

	if #list == 0 then
		return false
	end

	local ok = focus_in_list(list, dir)
	if not ok then
		ok = restore_minimized_on_screen(s)
	end

	local Solo = solo_api()
	if t and Solo and type(Solo.apply_for_tag) == "function" then
		local focused = client.focus
		gears.timer.delayed_call(function()
			Solo.apply_for_tag(t, focused or client.focus, is_max_layout)
		end)
	end

	return ok
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(args)
	args = args or {}

	runtime.ctx = args
	runtime.max_solo = ((cfg().tags or {}).max_solo == true)

	runtime.solo = require("shell.workspaces.policies.max_solo")

	if runtime.solo and type(runtime.solo.init_signals) == "function" then
		runtime.solo.init_signals({
			ctx = ctx(),
			is_max_layout = is_max_layout,
		})
	end

	return M
end

function M.current_screen()
	return current_nav_screen()
end

function M.is_max_layout(s)
	return is_max_layout(s)
end

function M.max_solo_enabled()
	return runtime.max_solo == true
end

function M.focus_client(dir, s)
	s = s or current_nav_screen()

	if not is_max_layout(s) then
		return false
	end

	if runtime.max_solo == true then
		return solo_mode(dir, s)
	end

	return cycle_mode(dir, s)
end

function M.handle_focus_navigation(dir)
	return M.focus_client(dir, current_nav_screen())
end

function M.handle_swap_navigation(_dir)
	local s = current_nav_screen()

	if is_max_layout(s) then
		return true
	end

	return false
end

function M.apply_current(s)
	local Solo = solo_api()
	if runtime.max_solo == true and Solo and type(Solo.apply_from_screen) == "function" then
		Solo.apply_from_screen(s or current_nav_screen(), is_max_layout)
	end
end

return M
