-- ~/.config/awesome/shell/workspaces/runtime/actions.lua
local awful = require("awful")

local M = {}

local runtime = {
	workspaces = {},
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function workspaces()
	return runtime.workspaces or {}
end

local function scr_in_dir_fn()
	local windowing = workspaces().windowing or nil
	local actions = windowing and windowing.actions or nil
	return actions and actions.scr_in_dir or nil
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(args)
	args = args or {}
	runtime.workspaces = args.workspaces or args or {}
	return M
end

-- =========================================================================
-- Tag
-- =========================================================================

function M.view_tag_idx(delta, s)
	s = s or awful.screen.focused()

	if not s or not s.selected_tag then
		return
	end

	awful.tag.viewidx(delta, s)
end

function M.move_tag_to_screen(dir)
	local s = awful.screen.focused()

	if not s or not s.selected_tag then
		return
	end

	local scr_in_dir = scr_in_dir_fn()
	if type(scr_in_dir) ~= "function" then
		return
	end

	local target = scr_in_dir(dir)
	if not target then
		return
	end

	local t = s.selected_tag
	t.screen = target
	t:view_only()

	awful.screen.focus(target)
end

-- =========================================================================
-- Client
-- =========================================================================

function M.move_client_to_neighbor_tag(delta, follow)
	local s = awful.screen.focused()

	if not s or not s.selected_tag then
		return
	end

	local tags = s.tags or {}
	local idx = (s.selected_tag.index or 1) + delta
	local target_tag = tags[idx]
	local c = client.focus

	if c and target_tag then
		c:move_to_tag(target_tag)

		if follow then
			target_tag:view_only()
			client.focus = c
			c:raise()
		end
	end
end

return M
