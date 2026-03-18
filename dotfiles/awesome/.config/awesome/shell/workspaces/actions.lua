-- shell/workspaces/actions.lua
local awful = require("awful")

local Windowing = require("shell.windowing.actions")

local M = {}

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

	local target = Windowing.scr_in_dir(dir)
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

	local tags = s.tags
	local idx = (s.selected_tag.index or 1) + delta
	local nt = tags[idx]
	local c = client.focus

	if c and nt then
		c:move_to_tag(nt)

		if follow then
			nt:view_only()
			client.focus = c
			c:raise()
		end
	end
end

return M
