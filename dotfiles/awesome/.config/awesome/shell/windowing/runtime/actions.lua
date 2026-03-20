-- ~/.config/awesome/shell/windowing/runtime/actions.lua
local awful = require("awful")

local M = {}

local runtime_api = {}

-- =========================================================================
-- Helpers
-- =========================================================================

local function state_api()
	return runtime_api.state
end

-- =========================================================================
-- Init
-- =========================================================================

function M.init(o)
	o = o or {}
	runtime_api = o.api or {}
end

-- =========================================================================
-- Screen
-- =========================================================================

function M.scr_in_dir(dir)
	local s = awful.screen.focused()
	return s and s:get_next_in_direction(dir) or nil
end

-- =========================================================================
-- Client
-- =========================================================================

function M.move_client_dir(dir)
	local c = client.focus
	if not c then
		return
	end

	awful.client.swap.bydirection(dir, c, nil)
end

function M.move_client_to_screen(dir)
	local c = client.focus
	if not c then
		return
	end

	local target = M.scr_in_dir(dir)
	if not target then
		return
	end

	local t = target.selected_tag or (target.tags and target.tags[1])

	c:move_to_screen(target)

	if t then
		c:move_to_tag(t)
		t:view_only()
	end

	awful.screen.focus(target)
	client.focus = c
	c:raise()
end

-- =========================================================================
-- Layout State
-- =========================================================================

function M.layout_state_mode(cfg)
	local State = state_api()

	if State and type(State.layout_state_mode) == "function" then
		return State.layout_state_mode(cfg)
	end

	local tags_cfg = (cfg and cfg.tags) or {}
	local layouts_cfg = tags_cfg.layouts or {}
	local mode = tostring(layouts_cfg.mode or "tiling"):lower()

	if mode == "floating" then
		return "maximized"
	end

	return "floating"
end

function M.is_layout_state_active(c, cfg)
	local State = state_api()

	if State and type(State.is_layout_state_active) == "function" then
		return State.is_layout_state_active(c, cfg)
	end

	if not (c and c.valid) then
		return false
	end

	local mode = M.layout_state_mode(cfg)

	if mode == "maximized" then
		return c.maximized == true
	end

	return c.floating == true
end

function M.toggle_layout_state(c, cfg)
	if not (c and c.valid) then
		return
	end

	local mode = M.layout_state_mode(cfg)

	if mode == "maximized" then
		c.maximized = not c.maximized
		c:raise()
		return
	end

	awful.client.floating.toggle(c)
	c:raise()
end

return M
