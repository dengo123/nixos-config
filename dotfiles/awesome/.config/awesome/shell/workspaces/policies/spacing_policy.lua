-- ~/.config/awesome/shell/workspaces/policies/spacing_policy.lua
local awful = require("awful")
local beautiful = require("beautiful")

local M = {}

local runtime_cfg = {}

-- =========================================================================
-- Helpers
-- =========================================================================

local function tags_cfg()
	return runtime_cfg.tags or {}
end

local function resolve_gap()
	local gap = tonumber(tags_cfg().gap)
	assert(gap ~= nil, "workspaces.spacing_policy: cfg.tags.gap fehlt/ungueltig")
	return gap
end

local function apply_beautiful_spacing()
	local gap = resolve_gap()

	beautiful.useless_gap = gap
	beautiful.gap_single_client = true
	beautiful.max_pad_on = true
	beautiful.max_pad_same_as_gap = true
end

-- =========================================================================
-- Apply
-- =========================================================================

local function apply(s)
	if not s then
		return
	end

	if not s.selected_tag then
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

function M.init(cfg)
	runtime_cfg = cfg or {}

	apply_beautiful_spacing()

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
