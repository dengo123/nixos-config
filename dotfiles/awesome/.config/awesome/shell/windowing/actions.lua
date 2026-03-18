-- ~/.config/awesome/shell/windowing/actions.lua
local awful = require("awful")
local gtable = require("gears.table")

local M = {}

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
-- Pseudo Maximize
-- =========================================================================

local function save_prev_geom(c)
	if not (c and c.valid) then
		return
	end

	local key = "_prev_geom_s" .. tostring(c.screen.index or 0)

	if not c[key] then
		c[key] = c:geometry()
	end
end

local function pop_prev_geom(c)
	if not (c and c.valid) then
		return
	end

	local key = "_prev_geom_s" .. tostring(c.screen.index or 0)
	local g = c[key]

	c[key] = nil

	return g
end

function M.pseudo_maximize(c, opts)
	if not (c and c.valid) then
		return
	end

	local defaults = {
		honor_workarea = true,
		margins = 0,
	}

	local merged = gtable.crush({}, defaults, true)

	if opts then
		gtable.crush(merged, opts, true)
	end

	save_prev_geom(c)

	c.floating = true
	c.maximized = false
	c.maximized_horizontal = false
	c.maximized_vertical = false

	awful.placement.maximize(c, merged)

	c.maximized_fake = true
	c:raise()

	return c
end

function M.unpseudo_maximize(c)
	if not (c and c.valid) then
		return
	end

	local g = pop_prev_geom(c)

	c.maximized_fake = false

	if g then
		c:geometry(g)
	end

	c:raise()

	return c
end

function M.toggle_pseudo_maximize(c, opts)
	if not (c and c.valid) then
		return
	end

	if c.maximized_fake then
		return M.unpseudo_maximize(c)
	end

	return M.pseudo_maximize(c, opts)
end

function M.clear_prev_geom_for_screen_change(c)
	if not (c and c.valid) then
		return
	end

	for i = 1, 16 do
		c["_prev_geom_s" .. i] = nil
	end
end

return M
