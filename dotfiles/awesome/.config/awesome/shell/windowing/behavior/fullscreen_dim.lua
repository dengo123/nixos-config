-- ~/.config/awesome/shell/windowing/behavior/fullscreen_dim.lua
local awful = require("awful")
local beautiful = require("beautiful")
local gears = require("gears")
local wibox = require("wibox")

local M = {}

local ENABLED = true
local DIM_BG = "#000000F2"
local NEVER_DIM_PRIMARY = false

local overlays = {}
local pending_update = nil
local signals_ready = false

-- =========================================================================
-- Helpers
-- =========================================================================

local function hide_all()
	for _, o in pairs(overlays) do
		if o and o.valid then
			o.visible = false
		end
	end
end

local function ensure_overlay(s)
	if overlays[s] and overlays[s].valid then
		return overlays[s]
	end

	local g = s.geometry
	local o = wibox({
		screen = s,
		visible = false,
		ontop = true,
		type = "splash",
		bg = DIM_BG,
		x = g.x,
		y = g.y,
		width = g.width,
		height = g.height,
	})

	o.input_passthrough = true
	overlays[s] = o

	return o
end

local function find_fullscreen_target()
	local fc = client.focus

	if fc and fc.valid and fc.fullscreen then
		return fc.screen, (fc._fullscreen_dim == true)
	end

	for c in
		awful.client.iterate(function(x)
			return x and x.valid and x.fullscreen
		end)
	do
		return c.screen, (c._fullscreen_dim == true)
	end

	return nil, false
end

local function do_update()
	if not ENABLED then
		hide_all()
		return
	end

	local fs_screen, active_dim = find_fullscreen_target()

	if not fs_screen or not active_dim then
		hide_all()
		return
	end

	local primary = awful.screen.primary

	for s, o in pairs(overlays) do
		if o and o.valid then
			local should_dim = (s ~= fs_screen) and not (NEVER_DIM_PRIMARY and s == primary)
			o.bg = DIM_BG
			o.visible = should_dim
		end
	end
end

local function schedule_update()
	if pending_update then
		pending_update:again()
		return
	end

	pending_update = gears.timer({
		timeout = 0.05,
		autostart = true,
		single_shot = true,
		callback = function()
			pending_update = nil
			pcall(do_update)
		end,
	})
end

local function setup_signals()
	if signals_ready then
		return
	end

	client.connect_signal("property::fullscreen", schedule_update)
	client.connect_signal("focus", schedule_update)

	client.connect_signal("unmanage", function(c)
		if c then
			c._fullscreen_dim = nil
		end
		schedule_update()
	end)

	tag.connect_signal("property::selected", schedule_update)

	screen.connect_signal("added", function(s)
		ensure_overlay(s)
		gears.timer.delayed_call(schedule_update)
	end)

	screen.connect_signal("removed", function(s)
		local o = overlays[s]

		if o and o.valid then
			o.visible = false
			pcall(function()
				o:setup(nil)
			end)
		end

		overlays[s] = nil
		gears.timer.delayed_call(schedule_update)
	end)

	signals_ready = true
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(opts)
	opts = opts or {}

	if opts.enabled ~= nil then
		ENABLED = (opts.enabled == true)
	end

	DIM_BG = opts.dim_bg or beautiful.dim_overlay_bg or DIM_BG

	if opts.never_dim_primary ~= nil then
		NEVER_DIM_PRIMARY = (opts.never_dim_primary == true)
	end

	setup_signals()

	for s in screen do
		ensure_overlay(s)
	end

	if not ENABLED then
		hide_all()
		return
	end

	gears.timer.delayed_call(schedule_update)
end

return M
