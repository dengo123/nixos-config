-- ~/.config/awesome/shell/windowing/behavior/navigation.lua
local awful = require("awful")

local M = {}

local runtime = {
	ctx = {},
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function ctx()
	return runtime.ctx or {}
end

local function current_nav_screen()
	local focused = awful.screen.focused()
	if focused and focused.valid then
		return focused
	end

	if client.focus and client.focus.valid and not client.focus.minimized and client.focus.screen then
		return client.focus.screen
	end

	return awful.screen.primary()
end

local function kbd_intent(ms)
	awesome.emit_signal("focus_policy::keyboard_intent", ms or 250)
end

local function max_policy()
	local c = ctx()

	return (c.policy and c.policy.max_policy)
		or (c.workspaces and c.workspaces.policy and c.workspaces.policy.max_policy)
		or nil
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(args)
	runtime.ctx = args or {}
	return M
end

function M.focus_client(dir)
	local Policy = max_policy()

	if Policy and type(Policy.handle_focus_navigation) == "function" then
		if Policy.handle_focus_navigation(dir) == true then
			return
		end
	end

	kbd_intent()
	awful.client.focus.bydirection(dir)
end

function M.swap_client(dir)
	local Policy = max_policy()

	if Policy and type(Policy.handle_swap_navigation) == "function" then
		if Policy.handle_swap_navigation(dir) == true then
			return
		end
	end

	kbd_intent()
	awful.client.swap.bydirection(dir)
end

function M.current_screen()
	local Policy = max_policy()

	if Policy and type(Policy.current_screen) == "function" then
		return Policy.current_screen()
	end

	return current_nav_screen()
end

function M.is_max_layout(s)
	local Policy = max_policy()

	if Policy and type(Policy.is_max_layout) == "function" then
		return Policy.is_max_layout(s)
	end

	return false
end

return M
