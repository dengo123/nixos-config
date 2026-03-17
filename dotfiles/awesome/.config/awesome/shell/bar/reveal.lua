-- ~/.config/awesome/shell/bar/reveal.lua
local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")

local M = {}

local triggers = {}
local bars = {}
local opts_by_screen = {}
local hide_timers = {}
local pending_update = nil
local signals_ready = false

-- =========================================================================
-- Helpers
-- =========================================================================

local function edge_geom(s, edge, trigger_px)
	local g = s.geometry

	if edge == "top" then
		return {
			x = g.x,
			y = g.y,
			width = g.width,
			height = trigger_px,
		}
	end

	if edge == "left" then
		return {
			x = g.x,
			y = g.y,
			width = trigger_px,
			height = g.height,
		}
	end

	if edge == "right" then
		return {
			x = g.x + g.width - trigger_px,
			y = g.y,
			width = trigger_px,
			height = g.height,
		}
	end

	return {
		x = g.x,
		y = g.y + g.height - trigger_px,
		width = g.width,
		height = trigger_px,
	}
end

local function screen_has_visible_fullscreen(s)
	local selected = s.selected_tag
	if not selected then
		return false
	end

	for _, c in ipairs(selected:clients() or {}) do
		if c and c.valid and c.fullscreen and not c.hidden and not c.minimized then
			return true
		end
	end

	return false
end

local function cancel_hide_timer(s)
	local t = hide_timers[s]
	if t then
		t:stop()
		hide_timers[s] = nil
	end
end

local function show_bar(s)
	local bar = bars[s]
	if not (bar and bar.valid) then
		return
	end

	cancel_hide_timer(s)
	bar.ontop = true
	bar.visible = true
end

local function hide_bar_now(s)
	local bar = bars[s]
	if not (bar and bar.valid) then
		return
	end

	cancel_hide_timer(s)

	if screen_has_visible_fullscreen(s) then
		bar.visible = false
	else
		bar.visible = true
	end
end

local function hide_bar_later(s)
	local bar = bars[s]
	local opts = opts_by_screen[s] or {}

	if not (bar and bar.valid) then
		return
	end

	if not screen_has_visible_fullscreen(s) then
		return
	end

	cancel_hide_timer(s)

	hide_timers[s] = gears.timer({
		timeout = tonumber(opts.hide_delay) or 0.20,
		autostart = true,
		single_shot = true,
		callback = function()
			hide_timers[s] = nil
			hide_bar_now(s)
		end,
	})
end

local function ensure_trigger(s)
	local bar = bars[s]
	local opts = opts_by_screen[s] or {}

	if not (bar and bar.valid) then
		return nil
	end

	local trigger = triggers[s]
	if trigger and trigger.valid then
		return trigger
	end

	trigger = wibox({
		screen = s,
		visible = false,
		ontop = true,
		opacity = 0,
		bg = "#00000000",
		type = "dock",
	})

	trigger:connect_signal("mouse::enter", function()
		show_bar(s)
	end)

	trigger:connect_signal("mouse::leave", function()
		hide_bar_later(s)
	end)

	bar:connect_signal("mouse::enter", function()
		cancel_hide_timer(s)
	end)

	bar:connect_signal("mouse::leave", function()
		hide_bar_later(s)
	end)

	triggers[s] = trigger
	return trigger
end

local function update_trigger_geom(s)
	local trigger = triggers[s]
	local opts = opts_by_screen[s] or {}

	if not (trigger and trigger.valid) then
		return
	end

	local edge = opts.edge or "bottom"
	local trigger_px = tonumber(opts.trigger_px) or 2
	local g = edge_geom(s, edge, trigger_px)

	trigger.x = g.x
	trigger.y = g.y
	trigger.width = g.width
	trigger.height = g.height
end

local function sync_screen(s)
	local bar = bars[s]
	local trigger = ensure_trigger(s)

	if not (bar and bar.valid and trigger and trigger.valid) then
		return
	end

	update_trigger_geom(s)

	local fullscreen_visible = screen_has_visible_fullscreen(s)

	trigger.visible = fullscreen_visible

	if fullscreen_visible then
		bar.ontop = true
		bar.visible = false
		pcall(function()
			bar:struts(nil)
		end)
	else
		cancel_hide_timer(s)
		bar.visible = true
	end
end

local function do_update()
	for s in pairs(bars) do
		if s and s.valid then
			sync_screen(s)
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

-- =========================================================================
-- Public API
-- =========================================================================

function M.attach(s, bar, opts)
	opts = opts or {}

	-- ---------------------------------------------------------------------
	-- Config
	-- ---------------------------------------------------------------------

	bars[s] = bar
	opts_by_screen[s] = {
		edge = opts.edge or "bottom",
		trigger_px = tonumber(opts.trigger_px) or 2,
		hide_delay = tonumber(opts.hide_delay) or 0.20,
	}

	-- ---------------------------------------------------------------------
	-- Setup
	-- ---------------------------------------------------------------------

	ensure_trigger(s)
	update_trigger_geom(s)
	schedule_update()
end

function M.init_signals()
	if signals_ready then
		return
	end

	-- ---------------------------------------------------------------------
	-- Client Signals
	-- ---------------------------------------------------------------------

	client.connect_signal("property::fullscreen", schedule_update)
	client.connect_signal("property::minimized", schedule_update)
	client.connect_signal("property::hidden", schedule_update)
	client.connect_signal("manage", schedule_update)
	client.connect_signal("unmanage", schedule_update)

	-- ---------------------------------------------------------------------
	-- Tag Signals
	-- ---------------------------------------------------------------------

	tag.connect_signal("property::selected", schedule_update)

	-- ---------------------------------------------------------------------
	-- Screen Signals
	-- ---------------------------------------------------------------------

	screen.connect_signal("property::geometry", function(s)
		if bars[s] then
			update_trigger_geom(s)
			schedule_update()
		end
	end)

	screen.connect_signal("removed", function(s)
		local trigger = triggers[s]

		if trigger and trigger.valid then
			trigger.visible = false
			pcall(function()
				trigger:setup(nil)
			end)
		end

		cancel_hide_timer(s)
		triggers[s] = nil
		bars[s] = nil
		opts_by_screen[s] = nil
	end)

	-- ---------------------------------------------------------------------
	-- Initial
	-- ---------------------------------------------------------------------

	signals_ready = true
	gears.timer.delayed_call(schedule_update)
end

return M
