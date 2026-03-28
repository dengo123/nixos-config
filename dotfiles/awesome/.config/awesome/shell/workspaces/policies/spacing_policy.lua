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

local function max_cfg()
	local tags = tags_cfg()
	local max = tags.max or {}

	return {
		padding = (max.padding == true),
	}
end

local function resolve_gap()
	local gap = tonumber(tags_cfg().gap)
	assert(gap ~= nil, "workspaces.spacing_policy: cfg.tags.gap fehlt/ungueltig")
	return gap
end

local function is_max_layout(layout)
	return layout == awful.layout.suit.max or layout == awful.layout.suit.max.fullscreen
end

local function apply_beautiful_spacing()
	local gap = resolve_gap()

	beautiful.useless_gap = gap
	beautiful.gap_single_client = true
	beautiful.max_pad_on = true
	beautiful.max_pad_same_as_gap = true
end

local function clear_screen_padding(s)
	if not s then
		return
	end

	s.padding = nil
end

local function apply_max_padding(s)
	if not s then
		return
	end

	local gap = resolve_gap()

	s.padding = {
		left = gap,
		right = gap,
		top = gap,
		bottom = gap,
	}
end

-- =========================================================================
-- Apply
-- =========================================================================

local function apply(s)
	if not s then
		return
	end

	local t = s.selected_tag
	if not t then
		clear_screen_padding(s)
		return
	end

	apply_beautiful_spacing()

	if is_max_layout(t.layout) and max_cfg().padding == true then
		apply_max_padding(s)
		return
	end

	clear_screen_padding(s)
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

	return M
end

return M
