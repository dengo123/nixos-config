-- ~/.config/awesome/system/session_state/restore.lua
local awful = require("awful")
local gears = require("gears")

local M = {}

local runtime = {
	ctx = {},
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function ctx()
	return runtime.ctx or {}
end

local function screen_key(s)
	if s and type(s.outputs) == "table" then
		for name, active in pairs(s.outputs) do
			if active then
				return name
			end
		end
	end

	return s and ("screen:" .. tostring(s.index)) or "screen:?"
end

local function screen_by_key(key)
	for s in screen do
		if screen_key(s) == key then
			return s
		end
	end

	return nil
end

local function safe_get_tag_by_name(s, name)
	if not (s and s.tags and name) then
		return nil
	end

	for _, t in ipairs(s.tags) do
		if t and t.name == name then
			return t
		end
	end

	return nil
end

local function safe_get_tag_by_idx(s, idx)
	if not (s and s.tags and idx) then
		return nil
	end

	return s.tags[idx]
end

local function layout_by_name(name)
	local suit = awful.layout.suit

	local map = {
		tile = suit.tile,
		tile_left = suit.tile.left,
		tile_bottom = suit.tile.bottom,
		tile_top = suit.tile.top,
		fair = suit.fair,
		fair_horizontal = suit.fair.horizontal,
		spiral = suit.spiral,
		dwindle = suit.spiral.dwindle,
		max = suit.max,
		max_fullscreen = suit.max.fullscreen,
		magnifier = suit.magnifier,
		corner_nw = suit.corner.nw,
		corner_ne = suit.corner.ne,
		corner_sw = suit.corner.sw,
		corner_se = suit.corner.se,
		floating = suit.floating,
	}

	return map[tostring(name or "")] or suit.tile
end

local function apply_screen_state(data)
	for s in screen do
		local ss = data.screens and data.screens[screen_key(s)] or nil
		if ss then
			local t = ss.selected_tag_name and safe_get_tag_by_name(s, ss.selected_tag_name) or nil

			if not t and ss.selected_tag_idx then
				t = safe_get_tag_by_idx(s, ss.selected_tag_idx)
			end

			if t and s.selected_tag ~= t then
				t:view_only()
			end

			if t and ss.layout_name then
				awful.layout.set(layout_by_name(ss.layout_name), t)
			end
		end
	end
end

local function should_preserve_rule_floating(c)
	return c.portrait_autosize == true or c.centered_autosize == true or c.type == "dialog" or c.type == "utility"
end

local function apply_client_state(data, opts)
	opts = opts or {}

	local restore_screen = opts.restore_screen == true
	local restore_tag = opts.restore_tag == true
	local restore_floating = opts.restore_floating == true
	local restore_state = (opts.restore_state ~= false)

	local by_window = {}

	for _, cs in ipairs(data.clients or {}) do
		if cs.window ~= nil then
			by_window[cs.window] = cs
		end
	end

	for _, c in ipairs(client.get()) do
		local cs = by_window[c.window]

		if cs then
			if restore_screen then
				local target_screen = cs.screen_key and screen_by_key(cs.screen_key) or nil
				if target_screen and target_screen.valid and c.screen ~= target_screen then
					c.screen = target_screen
				end
			end

			if restore_tag and c.screen then
				local t = cs.tag_name and safe_get_tag_by_name(c.screen, cs.tag_name) or nil

				if not t and cs.tag_idx then
					t = safe_get_tag_by_idx(c.screen, cs.tag_idx)
				end

				if t then
					c:move_to_tag(t)
				end
			end

			if restore_state then
				c.minimized = cs.minimized == true
				c.maximized = cs.maximized == true
				c.fullscreen = cs.fullscreen == true
			end

			if restore_floating and not should_preserve_rule_floating(c) then
				c.floating = cs.floating == true
			end
		end
	end
end

local function restore_pass(data, opts)
	apply_screen_state(data)
	apply_client_state(data, opts)
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(args)
	runtime.ctx = (args and (args.ctx or args)) or {}
	return M
end

function M.run(data, opts)
	if type(data) ~= "table" then
		return false
	end

	opts = opts or {}

	restore_pass(data, opts)

	gears.timer.start_new(0.30, function()
		restore_pass(data, opts)
		return false
	end)

	gears.timer.start_new(0.90, function()
		restore_pass(data, opts)
		return false
	end)

	gears.timer.start_new(1.80, function()
		restore_pass(data, opts)
		return false
	end)

	awesome.emit_signal("ui::wallpaper_refresh")
	return true
end

function M.restore_on_start(fn)
	gears.timer.start_new(0.25, function()
		if type(fn) == "function" then
			fn()
		end
		return false
	end)
end

return M
