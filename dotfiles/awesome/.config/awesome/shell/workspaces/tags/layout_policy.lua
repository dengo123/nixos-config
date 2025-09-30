-- ~/.config/awesome/features/workspaces/tags/layout_policy.lua
local awful = require("awful")

local M = {}

local function desired_layout_for(s)
	local g = s.geometry
	return (g.width >= g.height) and awful.layout.suit.tile or awful.layout.suit.tile.top
end

function M.apply_layout_policy(s)
	s = s or awful.screen.focused()
	if not s then
		return
	end
	local desired = desired_layout_for(s)
	if s.selected_tag and s.selected_tag.layout ~= desired then
		s.selected_tag.layout = desired
	end
end

function M.apply_layout_policy_all(s)
	s = s or awful.screen.focused()
	if not s then
		return
	end
	local desired = desired_layout_for(s)
	for _, t in ipairs(s.tags or {}) do
		if t.layout ~= desired then
			t.layout = desired
		end
	end
end

function M.on_screen_rotation()
	screen.connect_signal("property::geometry", function(s)
		M.apply_layout_policy_all(s)
	end)
end

return M
