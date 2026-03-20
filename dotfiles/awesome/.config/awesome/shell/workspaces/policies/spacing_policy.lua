-- ~/.config/awesome/shell/workspaces/policies/spacing_policy.lua
local awful = require("awful")
local beautiful = require("beautiful")
local gears = require("gears")

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

local function is_max_layout(layout)
	return layout == awful.layout.suit.max or layout == awful.layout.suit.max.fullscreen
end

local function is_max_fullscreen_layout(layout)
	return layout == awful.layout.suit.max.fullscreen
end

local function apply_beautiful_spacing(layout)
	local gap = resolve_gap()

	beautiful.useless_gap = gap
	beautiful.gap_single_client = true
	beautiful.max_pad_on = true
	beautiful.max_pad_same_as_gap = true
end

-- debug

-- =========================================================================
-- Apply
-- =========================================================================

local function apply(s)
	if not s then
		return
	end

	local t = s.selected_tag
	if not t then
		awful.screen.padding(s, nil)
		return
	end

	apply_beautiful_spacing(t.layout)

	if is_max_layout(t.layout) then
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
