-- ~/.config/awesome/shell/workspaces/policies/spacing_policy.lua
local awful = require("awful")
local beautiful = require("beautiful")

local M = {}

-- =========================================================================
-- Helpers
-- =========================================================================

local function resolve_gap()
	local gap = tonumber(beautiful.useless_gap)
	assert(gap ~= nil, "workspaces.spacing_policy: beautiful.useless_gap fehlt/ungueltig")
	return gap
end

local function is_enabled()
	return beautiful.max_pad_on ~= false
end

-- =========================================================================
-- Apply
-- =========================================================================

local function apply(s)
	if not s then
		return
	end

	if not is_enabled() or not s.selected_tag then
		awful.screen.padding(s, nil)
		return
	end

	if s.selected_tag.layout == awful.layout.suit.max then
		local gap = resolve_gap()

		awful.screen.padding(s, {
			left = gap,
			right = gap,
			top = gap,
			bottom = gap,
		})

		return
	end

	awful.screen.padding(s, nil)
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init()
	awful.screen.connect_for_each_screen(function(s)
		apply(s)
	end)

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
