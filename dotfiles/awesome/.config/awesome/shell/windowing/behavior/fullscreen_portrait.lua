-- ~/.config/awesome/shell/windowing/behavior/fullscreen_portrait.lua
local awful = require("awful")
local gears = require("gears")

local M = {}

local runtime_cfg = {}
local signals_ready = false

-- =========================================================================
-- Helpers
-- =========================================================================

local function layouts_cfg()
	local tags_cfg = runtime_cfg.tags or {}
	return tags_cfg.layouts or {}
end

local function is_enabled()
	return layouts_cfg().use_max_fullscreen_for_portrait == true
end

local function has_tag(c, t)
	if not (c and c.valid and t) then
		return false
	end

	for _, ct in ipairs(c:tags() or {}) do
		if ct == t then
			return true
		end
	end

	return false
end

local function selected_tag_uses_max_fullscreen_layout(s)
	local t = s and s.selected_tag
	return t and t.layout == awful.layout.suit.max.fullscreen or false
end

local function client_should_auto_fullscreen(c)
	if not (c and c.valid) then
		return false
	end

	if not is_enabled() then
		return false
	end

	if c.floating then
		return false
	end

	local s = c.screen
	if not s then
		return false
	end

	local selected = s.selected_tag
	if not selected then
		return false
	end

	if not selected_tag_uses_max_fullscreen_layout(s) then
		return false
	end

	if not has_tag(c, selected) then
		return false
	end

	return true
end

local function apply_client(c)
	if not (c and c.valid) then
		return
	end

	local should_fullscreen = client_should_auto_fullscreen(c)

	if should_fullscreen then
		if c.fullscreen ~= true then
			c._auto_portrait_fullscreen = true
			c.fullscreen = true
			c:raise()
		else
			c._auto_portrait_fullscreen = true
		end

		return
	end

	if c._auto_portrait_fullscreen == true then
		c._auto_portrait_fullscreen = nil

		if c.valid then
			c.fullscreen = false
		end
	end
end

local function apply_screen(s)
	if not (s and s.valid) then
		return
	end

	for _, c in ipairs(s.clients or {}) do
		apply_client(c)
	end
end

local function delayed_apply_client(c)
	gears.timer.delayed_call(function()
		apply_client(c)
	end)
end

local function delayed_apply_screen(s)
	gears.timer.delayed_call(function()
		apply_screen(s)
	end)
end

local function setup_signals()
	if signals_ready then
		return
	end

	-- ---------------------------------------------------------------------
	-- Client Signals
	-- ---------------------------------------------------------------------

	client.connect_signal("manage", function(c)
		delayed_apply_client(c)
	end)

	client.connect_signal("property::floating", function(c)
		delayed_apply_client(c)
	end)

	client.connect_signal("property::fullscreen", function(c)
		if c and c.valid and c._auto_portrait_fullscreen == true then
			delayed_apply_client(c)
		end
	end)

	client.connect_signal("tagged", function(c)
		delayed_apply_client(c)
	end)

	client.connect_signal("untagged", function(c)
		delayed_apply_client(c)
	end)

	client.connect_signal("unmanage", function(c)
		if c then
			c._auto_portrait_fullscreen = nil
		end
	end)

	-- ---------------------------------------------------------------------
	-- Tag Signals
	-- ---------------------------------------------------------------------

	tag.connect_signal("property::layout", function(t)
		if t and t.screen then
			delayed_apply_screen(t.screen)
		end
	end)

	tag.connect_signal("property::selected", function(t)
		if t and t.screen then
			delayed_apply_screen(t.screen)
		end
	end)

	-- ---------------------------------------------------------------------
	-- Screen Signals
	-- ---------------------------------------------------------------------

	screen.connect_signal("property::geometry", function(s)
		delayed_apply_screen(s)
	end)

	signals_ready = true
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(o)
	o = o or {}
	runtime_cfg = o.cfg or o or {}

	setup_signals()

	gears.timer.delayed_call(function()
		for s in screen do
			apply_screen(s)
		end
	end)
end

return M
