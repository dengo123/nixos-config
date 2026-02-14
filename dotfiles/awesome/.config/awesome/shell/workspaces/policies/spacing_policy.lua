-- shell/workspaces/policies/spacing_policy.lua
local awful = require("awful")
local beautiful = require("beautiful")

local M = {}

function M.init()
	local GAP = beautiful.useless_gap or 8
	local ENABLE = (beautiful.max_pad_on ~= false)

	local function apply(s)
		if not ENABLE or not s.selected_tag then
			awful.screen.padding(s, nil)
			return
		end

		if s.selected_tag.layout == awful.layout.suit.max then
			awful.screen.padding(s, {
				left = GAP,
				right = GAP,
				top = GAP,
				bottom = GAP,
			})
		else
			awful.screen.padding(s, nil)
		end
	end

	awful.screen.connect_for_each_screen(apply)
	tag.connect_signal("property::layout", function(t)
		if t and t.screen then
			apply(t.screen)
		end
	end)
	tag.connect_signal("property::selected", function(t)
		if t and t.screen then
			apply(t.screen)
		end
	end)
end

return M
