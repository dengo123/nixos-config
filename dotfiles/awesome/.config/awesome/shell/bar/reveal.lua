-- ~/.config/awesome/shell/bar/reveal.lua
local gears = require("gears")
local wibox = require("wibox")

local M = {}

local runtime = {
	ctx = {},
	triggers = {},
	bars = {},
	opts_by_screen = {},
	hide_timers = {},
	pending_update = nil,
	signals_ready = false,
	peek_until_by_screen = {},
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function ctx()
	return runtime.ctx or {}
end

local function triggers()
	return runtime.triggers
end

local function bars()
	return runtime.bars
end

local function opts_by_screen()
	return runtime.opts_by_screen
end

local function hide_timers()
	return runtime.hide_timers
end

local function peek_until_by_screen()
	return runtime.peek_until_by_screen
end

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

local function screen_has_visible_fullscreen_client(s)
	local selected = s and s.selected_tag
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

local function bar_normally_visible(s)
	local bar = bars()[s]
	return bar and bar.valid and bar._normal_visibility_enabled == true
end

local function reveal_active_for_screen(s)
	if bar_normally_visible(s) then
		return screen_has_visible_fullscreen_client(s)
	end

	return true
end

local function cancel_hide_timer(s)
	local t = hide_timers()[s]
	if t then
		t:stop()
		hide_timers()[s] = nil
	end
end

local function remaining_peek_delay(s)
	local until_ts = peek_until_by_screen()[s]
	if not until_ts then
		return 0
	end

	local now = os.clock()
	local remaining = until_ts - now

	if remaining <= 0 then
		peek_until_by_screen()[s] = nil
		return 0
	end

	return remaining
end

local function effective_hide_delay(s, delay)
	local normal_delay = tonumber(delay) or 0
	local peek_delay = remaining_peek_delay(s)
	return math.max(normal_delay, peek_delay)
end

local function show_bar(s)
	local bar = bars()[s]
	if not (bar and bar.valid) then
		return
	end

	cancel_hide_timer(s)
	bar.ontop = true
	bar.visible = true
end

local function hide_bar_now(s)
	local bar = bars()[s]
	if not (bar and bar.valid) then
		return
	end

	cancel_hide_timer(s)

	if remaining_peek_delay(s) > 0 then
		bar.visible = true
		hide_timers()[s] = gears.timer({
			timeout = remaining_peek_delay(s),
			autostart = true,
			single_shot = true,
			callback = function()
				hide_timers()[s] = nil
				peek_until_by_screen()[s] = nil
				hide_bar_now(s)
			end,
		})
		return
	end

	if reveal_active_for_screen(s) then
		bar.visible = false
	else
		bar.visible = bar_normally_visible(s)
	end
end

local function hide_bar_later(s, delay)
	local bar = bars()[s]
	local opts = opts_by_screen()[s] or {}

	if not (bar and bar.valid) then
		return
	end

	if not reveal_active_for_screen(s) then
		return
	end

	cancel_hide_timer(s)

	local final_delay = effective_hide_delay(s, tonumber(delay) or tonumber(opts.hide_delay) or 0.20)

	hide_timers()[s] = gears.timer({
		timeout = final_delay,
		autostart = true,
		single_shot = true,
		callback = function()
			hide_timers()[s] = nil
			if remaining_peek_delay(s) <= 0 then
				peek_until_by_screen()[s] = nil
			end
			hide_bar_now(s)
		end,
	})
end

local function peek_bar(s, duration)
	local bar = bars()[s]
	if not (bar and bar.valid) then
		return
	end

	local peek_duration = tonumber(duration) or 0.70
	peek_until_by_screen()[s] = os.clock() + peek_duration

	show_bar(s)
	hide_bar_later(s, peek_duration)
end

local function peek_for_screen(s)
	if not (s and s.valid) then
		return
	end

	local opts = opts_by_screen()[s] or {}
	if reveal_active_for_screen(s) then
		peek_bar(s, opts.layout_peek_duration)
	end
end

local function ensure_trigger(s)
	local bar = bars()[s]

	if not (bar and bar.valid) then
		return nil
	end

	local trigger = triggers()[s]
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

	triggers()[s] = trigger
	return trigger
end

local function update_trigger_geom(s)
	local trigger = triggers()[s]
	local opts = opts_by_screen()[s] or {}

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
	local bar = bars()[s]
	local trigger = ensure_trigger(s)

	if not (bar and bar.valid and trigger and trigger.valid) then
		return
	end

	update_trigger_geom(s)

	local reveal_active = reveal_active_for_screen(s)

	trigger.visible = reveal_active

	if remaining_peek_delay(s) > 0 then
		bar.ontop = true
		bar.visible = true
		pcall(function()
			bar:struts(nil)
		end)
		return
	end

	if reveal_active then
		bar.ontop = true
		bar.visible = false
		pcall(function()
			bar:struts(nil)
		end)
	else
		cancel_hide_timer(s)
		bar.visible = bar_normally_visible(s)
	end
end

local function do_update()
	for s in pairs(bars()) do
		if s and s.valid then
			sync_screen(s)
		end
	end
end

local function schedule_update()
	if runtime.pending_update then
		runtime.pending_update:again()
		return
	end

	runtime.pending_update = gears.timer({
		timeout = 0.05,
		autostart = true,
		single_shot = true,
		callback = function()
			runtime.pending_update = nil
			pcall(do_update)
		end,
	})
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(args)
	runtime.ctx = (args and (args.ctx or args)) or {}
	return M
end

function M.attach(s, bar, opts)
	opts = opts or {}

	bars()[s] = bar
	opts_by_screen()[s] = {
		edge = opts.edge or "bottom",
		trigger_px = tonumber(opts.trigger_px) or 2,
		hide_delay = tonumber(opts.hide_delay) or 0.20,
		layout_peek_duration = tonumber(opts.layout_peek_duration) or 0.70,
	}

	ensure_trigger(s)
	update_trigger_geom(s)
	schedule_update()
end

function M.init_signals()
	if runtime.signals_ready then
		return
	end

	client.connect_signal("property::fullscreen", schedule_update)
	client.connect_signal("property::minimized", schedule_update)
	client.connect_signal("property::hidden", schedule_update)
	client.connect_signal("manage", schedule_update)
	client.connect_signal("unmanage", schedule_update)

	client.connect_signal("focus", function(c)
		if not (c and c.screen) then
			return
		end

		schedule_update()
		gears.timer.delayed_call(function()
			peek_for_screen(c.screen)
		end)
	end)

	tag.connect_signal("property::layout", function(t)
		if not (t and t.screen) then
			return
		end

		schedule_update()
		gears.timer.delayed_call(function()
			peek_for_screen(t.screen)
		end)
	end)

	tag.connect_signal("property::selected", function(t)
		if not (t and t.screen and t.selected) then
			return
		end

		schedule_update()
		gears.timer.delayed_call(function()
			peek_for_screen(t.screen)
		end)
	end)

	screen.connect_signal("property::geometry", function(s)
		if bars()[s] then
			update_trigger_geom(s)
			schedule_update()
		end
	end)

	screen.connect_signal("removed", function(s)
		local trigger = triggers()[s]

		if trigger and trigger.valid then
			trigger.visible = false
			pcall(function()
				trigger:setup(nil)
			end)
		end

		cancel_hide_timer(s)
		triggers()[s] = nil
		bars()[s] = nil
		opts_by_screen()[s] = nil
		peek_until_by_screen()[s] = nil
	end)

	runtime.signals_ready = true
	gears.timer.delayed_call(schedule_update)
end

return M
