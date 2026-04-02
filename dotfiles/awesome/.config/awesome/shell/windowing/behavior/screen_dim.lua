-- ~/.config/awesome/shell/windowing/behavior/screen_dim.lua
local awful = require("awful")
local beautiful = require("beautiful")
local gears = require("gears")
local wibox = require("wibox")

local M = {}

local ENABLED = true
local DIM_BG = "#000000F2"
local DIM_PRIMARY = false

local overlays = {}
local active = false
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

local function update_overlay_geometry(s)
	local o = overlays[s]
	if not (s and o and o.valid) then
		return
	end

	local g = s.geometry
	o.x = g.x
	o.y = g.y
	o.width = g.width
	o.height = g.height
end

local function do_update()
	if not ENABLED or not active then
		hide_all()
		return
	end

	local primary = screen.primary or awful.screen.primary

	for s, o in pairs(overlays) do
		if o and o.valid then
			update_overlay_geometry(s)

			local should_dim = (s ~= primary) or (DIM_PRIMARY == true)
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

	screen.connect_signal("property::geometry", schedule_update)

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

	if opts.dim_primary ~= nil then
		DIM_PRIMARY = (opts.dim_primary == true)
	end

	setup_signals()

	for s in screen do
		ensure_overlay(s)
	end

	gears.timer.delayed_call(schedule_update)

	return M
end

function M.is_active()
	return active == true
end

function M.set_active(value)
	active = (value == true)
	schedule_update()
	return active
end

function M.toggle()
	return M.set_active(not active)
end

function M.show()
	return M.set_active(true)
end

function M.hide()
	return M.set_active(false)
end

return M
