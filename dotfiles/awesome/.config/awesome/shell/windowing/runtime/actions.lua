-- ~/.config/awesome/shell/windowing/runtime/actions.lua
local awful = require("awful")

local M = {}

local runtime = {
	windowing = {},
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function windowing()
	return runtime.windowing or {}
end

local function state_api()
	return windowing().state
end

local function focused_screen()
	return awful.screen.focused()
end

local function each_client_on_tag(t, fn)
	for _, c in ipairs((t and t:clients()) or {}) do
		if c and c.valid and not c.minimized then
			fn(c)
		end
	end
end

local function each_client_on_screen_tags(s, fn)
	if not (s and s.valid) then
		return
	end

	for _, t in ipairs(s.tags or {}) do
		each_client_on_tag(t, fn)
	end
end

local function each_client_on_visible_tag_of_screen(s, fn)
	if not (s and s.valid) then
		return
	end

	each_client_on_tag(s.selected_tag, fn)
end

local function minimize_client(c)
	if not (c and c.valid) then
		return
	end

	if c.minimized then
		return
	end

	awesome.emit_signal("ui::suppress_center", 0.2)
	c.minimized = true
end

-- =========================================================================
-- Init
-- =========================================================================

function M.init(args)
	args = args or {}
	runtime.windowing = args.windowing or args or {}
	return M
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

function M.minimize_focused()
	local c = client.focus
	if not c then
		return false
	end

	minimize_client(c)
	return true
end

function M.minimize_visible_tag_on_screen(s)
	s = s or focused_screen()
	if not (s and s.valid) then
		return false
	end

	each_client_on_visible_tag_of_screen(s, minimize_client)
	return true
end

function M.minimize_all_tags_on_screen(s)
	s = s or focused_screen()
	if not (s and s.valid) then
		return false
	end

	each_client_on_screen_tags(s, minimize_client)
	return true
end

function M.minimize_visible_tags_on_all_screens()
	for s in screen do
		each_client_on_visible_tag_of_screen(s, minimize_client)
	end

	return true
end

function M.minimize_all_tags_on_all_screens()
	for s in screen do
		each_client_on_screen_tags(s, minimize_client)
	end

	return true
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
