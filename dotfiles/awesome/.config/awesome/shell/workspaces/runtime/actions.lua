-- ~/.config/awesome/shell/workspaces/runtime/actions.lua
local awful = require("awful")

local M = {}

local deps = {
	scr_in_dir = nil,
}

-- =========================================================================
-- Dependencies
-- =========================================================================

function M.set_dependencies(value)
	value = value or {}

	if type(value.scr_in_dir) == "function" then
		deps.scr_in_dir = value.scr_in_dir
	end
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

	if type(deps.scr_in_dir) ~= "function" then
		return
	end

	local target = deps.scr_in_dir(dir)

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
