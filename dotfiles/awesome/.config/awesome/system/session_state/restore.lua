-- ~/.config/awesome/system/session_state/restore.lua
local awful = require("awful")
local gears = require("gears")

local M = {}

local runtime = {
	cfg = {},
	pending_clients = {},
	last_opts = {},
	manage_hook_ready = false,
	run_id = 0,
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function cfg()
	return runtime.cfg or {}
end

local function session_state_cfg()
	return ((cfg() or {}).system or {}).session_state or {}
end

local function preserve_rule_floating_cfg()
	return session_state_cfg().preserve_rule_floating or {}
end

local function preserve_rule_floating_enabled(name, default)
	local prf = preserve_rule_floating_cfg()
	local value = prf[name]

	if value == nil then
		return default == true
	end

	return value == true
end

local function screen_key(s)
	if s and type(s.outputs) == "table" then
		local names = {}

		for name, active in pairs(s.outputs) do
			if active then
				table.insert(names, tostring(name))
			end
		end

		table.sort(names)

		if #names > 0 then
			return table.concat(names, "+")
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

local function shallow_copy_clients(clients)
	local out = {}

	for _, cs in ipairs(clients or {}) do
		table.insert(out, cs)
	end

	return out
end

local function should_preserve_rule_floating(c)
	if not (c and c.valid) then
		return false
	end

	if preserve_rule_floating_enabled("dialog", true) and c.type == "dialog" then
		return true
	end

	if preserve_rule_floating_enabled("utility", true) and c.type == "utility" then
		return true
	end

	if preserve_rule_floating_enabled("portrait_autosize", true) and c.portrait_autosize == true then
		return true
	end

	if preserve_rule_floating_enabled("centered_autosize", true) and c.centered_autosize == true then
		return true
	end

	return false
end

local function apply_selected_tag_state(data, opts)
	if not (opts and opts.restore_tag == true) then
		return
	end

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
		end
	end
end

local function apply_layout_state(data, opts)
	if not (opts and opts.restore_layout ~= false) then
		return
	end

	for s in screen do
		local ss = data.screens and data.screens[screen_key(s)] or nil
		if ss and ss.layout_name then
			local t = nil

			if ss.selected_tag_name then
				t = safe_get_tag_by_name(s, ss.selected_tag_name)
			end

			if not t and ss.selected_tag_idx then
				t = safe_get_tag_by_idx(s, ss.selected_tag_idx)
			end

			if not t then
				t = s.selected_tag
			end

			if t then
				awful.layout.set(layout_by_name(ss.layout_name), t)
			end
		end
	end
end

local function apply_client_state_to_client(c, cs, opts)
	opts = opts or {}

	if not (c and c.valid and cs) then
		return false
	end

	local restore_screen = opts.restore_screen == true
	local restore_tag = opts.restore_tag == true
	local restore_state = (opts.restore_state ~= false)

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

		if not should_preserve_rule_floating(c) then
			c.floating = cs.floating == true
		end
	end

	return true
end

local function client_match_score(c, cs)
	local score = 0

	if not (c and c.valid and cs) then
		return score
	end

	if cs.window ~= nil and c.window == cs.window then
		return 10000
	end

	if cs.startup_id and c.startup_id and cs.startup_id == c.startup_id then
		score = score + 500
	end

	if cs.class and c.class == cs.class then
		score = score + 200
	end

	if cs.instance and c.instance == cs.instance then
		score = score + 120
	end

	if cs.role and c.role == cs.role then
		score = score + 80
	end

	if cs.type and c.type == cs.type then
		score = score + 40
	end

	if cs.name and c.name == cs.name then
		score = score + 25
	end

	if cs.pid and c.pid and cs.pid == c.pid then
		score = score + 10
	end

	return score
end

local function find_best_client_state(c, remaining)
	local best_idx = nil
	local best_score = 0

	for i, cs in ipairs(remaining or {}) do
		local score = client_match_score(c, cs)
		if score > best_score then
			best_score = score
			best_idx = i
		end
	end

	if not best_idx or best_score < 200 then
		return nil, nil
	end

	return best_idx, remaining[best_idx]
end

local function apply_pending_to_client(c, opts)
	local idx, cs = find_best_client_state(c, runtime.pending_clients)
	if not (idx and cs) then
		return false
	end

	if apply_client_state_to_client(c, cs, opts) then
		table.remove(runtime.pending_clients, idx)
		return true
	end

	return false
end

local function apply_client_state(data, opts)
	runtime.pending_clients = shallow_copy_clients(data.clients or {})
	runtime.last_opts = opts or {}

	for _, c in ipairs(client.get()) do
		apply_pending_to_client(c, runtime.last_opts)
	end
end

local function restore_pass(data, opts)
	apply_selected_tag_state(data, opts)
	apply_layout_state(data, opts)
	apply_client_state(data, opts)
end

local function ensure_manage_hook()
	if runtime.manage_hook_ready then
		return
	end

	runtime.manage_hook_ready = true

	client.connect_signal("manage", function(c)
		if #(runtime.pending_clients or {}) <= 0 then
			return
		end

		gears.timer.delayed_call(function()
			if not (c and c.valid) then
				return
			end

			apply_pending_to_client(c, runtime.last_opts or {})
		end)
	end)
end

local function schedule_restore_pass(run_id, delay, data, opts)
	gears.timer.start_new(delay, function()
		if run_id ~= runtime.run_id then
			return false
		end

		restore_pass(data, opts)
		return false
	end)
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(args)
	args = args or {}
	runtime.cfg = args.cfg or runtime.cfg or {}
	ensure_manage_hook()
	return M
end

function M.run(data, opts)
	if type(data) ~= "table" then
		return false
	end

	opts = opts or {}
	runtime.run_id = runtime.run_id + 1
	local run_id = runtime.run_id

	restore_pass(data, opts)

	if opts.retry_passes ~= false then
		schedule_restore_pass(run_id, 0.30, data, opts)
		schedule_restore_pass(run_id, 0.90, data, opts)
		schedule_restore_pass(run_id, 1.80, data, opts)
		schedule_restore_pass(run_id, 3.00, data, opts)
	end

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
