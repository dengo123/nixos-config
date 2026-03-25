-- ~/.config/awesome/system/session_state.lua
local awful = require("awful")
local gears = require("gears")
local gfs = require("gears.filesystem")

local M = {}

local STATE_PATH = gfs.get_cache_dir() .. "session-state.lua"

local runtime = {
	attached = false,
	restore_scheduled = false,
	save_timer = nil,
}

-- =========================================================================
-- Files / Serialization
-- =========================================================================

local function ensure_cache_dir()
	pcall(function()
		gfs.make_directories(gfs.get_cache_dir())
	end)
end

local function escape_string(s)
	s = tostring(s or "")
	s = s:gsub("\\", "\\\\")
	s = s:gsub("\n", "\\n")
	s = s:gsub("\r", "\\r")
	s = s:gsub("\t", "\\t")
	s = s:gsub('"', '\\"')
	return '"' .. s .. '"'
end

local function is_identifier(s)
	return type(s) == "string" and s:match("^[%a_][%w_]*$") ~= nil
end

local function serialize(value, seen)
	seen = seen or {}

	local t = type(value)

	if t == "nil" then
		return "nil"
	end

	if t == "boolean" then
		return value and "true" or "false"
	end

	if t == "number" then
		if value ~= value then
			return "0"
		end
		if value == math.huge or value == -math.huge then
			return "0"
		end
		return tostring(value)
	end

	if t == "string" then
		return escape_string(value)
	end

	if t ~= "table" then
		return "nil"
	end

	if seen[value] then
		return "nil"
	end
	seen[value] = true

	local out = {}
	local is_array = true
	local max_index = 0

	for k, _ in pairs(value) do
		if type(k) ~= "number" or k < 1 or k % 1 ~= 0 then
			is_array = false
			break
		end
		if k > max_index then
			max_index = k
		end
	end

	if is_array then
		for i = 1, max_index do
			table.insert(out, serialize(value[i], seen))
		end
		seen[value] = nil
		return "{ " .. table.concat(out, ", ") .. " }"
	end

	for k, v in pairs(value) do
		local key
		if is_identifier(k) then
			key = k
		else
			key = "[" .. serialize(k, seen) .. "]"
		end

		table.insert(out, key .. " = " .. serialize(v, seen))
	end

	table.sort(out)
	seen[value] = nil
	return "{ " .. table.concat(out, ", ") .. " }"
end

local function write_state(path, data)
	ensure_cache_dir()

	local payload = "return " .. serialize(data) .. "\n"

	local f = io.open(path, "w")
	if not f then
		return false
	end

	f:write(payload)
	f:close()
	return true
end

local function read_state(path)
	local chunk = loadfile(path)
	if not chunk then
		return nil
	end

	local ok, data = pcall(chunk)
	if not ok or type(data) ~= "table" then
		return nil
	end

	return data
end

-- =========================================================================
-- Screen / Tag / Layout helpers
-- =========================================================================

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

local function selected_tag(s)
	return s and s.selected_tag or nil
end

local function first_tag(c)
	return c and c.first_tag or nil
end

local function tag_name(t)
	return t and t.name or nil
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

local function current_layout_for_tag(t)
	if not t then
		return nil
	end

	return awful.layout.get(t.screen)
end

local function layout_name(layout_obj)
	local suit = awful.layout.suit

	local map = {
		[suit.tile] = "tile",
		[suit.tile.left] = "tile_left",
		[suit.tile.bottom] = "tile_bottom",
		[suit.tile.top] = "tile_top",
		[suit.fair] = "fair",
		[suit.fair.horizontal] = "fair_horizontal",
		[suit.spiral] = "spiral",
		[suit.spiral.dwindle] = "dwindle",
		[suit.max] = "max",
		[suit.max.fullscreen] = "max_fullscreen",
		[suit.magnifier] = "magnifier",
		[suit.corner.nw] = "corner_nw",
		[suit.corner.ne] = "corner_ne",
		[suit.corner.sw] = "corner_sw",
		[suit.corner.se] = "corner_se",
		[suit.floating] = "floating",
	}

	return map[layout_obj] or "tile"
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

-- =========================================================================
-- Snapshot
-- =========================================================================

local function current_state()
	local data = {
		version = 1,
		ts = os.time(),
		screens = {},
		clients = {},
	}

	for s in screen do
		local t = selected_tag(s)

		data.screens[screen_key(s)] = {
			selected_tag_name = tag_name(t),
			selected_tag_idx = t and t.index or nil,
			layout_name = t and layout_name(current_layout_for_tag(t)) or nil,
		}
	end

	for _, c in ipairs(client.get()) do
		local t = first_tag(c)

		table.insert(data.clients, {
			window = c.window,
			screen_key = c.screen and screen_key(c.screen) or nil,
			tag_name = tag_name(t),
			tag_idx = t and t.index or nil,
			minimized = c.minimized == true,
			floating = awful.client.floating.get(c) == true,
			fullscreen = c.fullscreen == true,
			maximized = c.maximized == true,
		})
	end

	return data
end

local function save_now()
	return write_state(STATE_PATH, current_state())
end

local function schedule_save()
	if runtime.save_timer then
		runtime.save_timer:again()
		return
	end

	runtime.save_timer = gears.timer({
		timeout = 0.75,
		autostart = true,
		single_shot = true,
		callback = function()
			runtime.save_timer = nil
			save_now()
		end,
	})
end

-- =========================================================================
-- Restore
-- =========================================================================

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

local function apply_client_state(data)
	local by_window = {}

	for _, cs in ipairs(data.clients or {}) do
		if cs.window ~= nil then
			by_window[cs.window] = cs
		end
	end

	for _, c in ipairs(client.get()) do
		local cs = by_window[c.window]

		if cs then
			local target_screen = cs.screen_key and screen_by_key(cs.screen_key) or nil

			if target_screen and target_screen.valid and c.screen ~= target_screen then
				c.screen = target_screen
			end

			if c.screen then
				local t = cs.tag_name and safe_get_tag_by_name(c.screen, cs.tag_name) or nil

				if not t and cs.tag_idx then
					t = safe_get_tag_by_idx(c.screen, cs.tag_idx)
				end

				if t then
					c:move_to_tag(t)
				end
			end

			c.minimized = cs.minimized == true
			awful.client.floating.set(c, cs.floating == true)
			c.maximized = cs.maximized == true
			c.fullscreen = cs.fullscreen == true
		end
	end
end

local function restore_pass(data)
	apply_screen_state(data)
	apply_client_state(data)
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.snapshot()
	return save_now()
end

function M.restore()
	local data = read_state(STATE_PATH)
	if not data then
		return false
	end

	restore_pass(data)

	gears.timer.start_new(0.30, function()
		restore_pass(data)
		return false
	end)

	gears.timer.start_new(0.90, function()
		restore_pass(data)
		return false
	end)

	gears.timer.start_new(1.80, function()
		restore_pass(data)
		return false
	end)

	awesome.emit_signal("ui::wallpaper_refresh")
	return true
end

function M.restore_on_start()
	if runtime.restore_scheduled then
		return
	end

	runtime.restore_scheduled = true

	gears.timer.start_new(0.25, function()
		runtime.restore_scheduled = false
		M.restore()
		return false
	end)
end

function M.attach_signals()
	if runtime.attached then
		return
	end

	runtime.attached = true

	awesome.connect_signal("autorandr::pre", function()
		M.snapshot()
	end)

	awesome.connect_signal("autorandr::applied", function()
		M.restore()
	end)

	awesome.connect_signal("session::pre_change", function()
		M.snapshot()
	end)

	awesome.connect_signal("session::post_change", function()
		M.restore()
	end)

	tag.connect_signal("property::selected", schedule_save)
	tag.connect_signal("property::layout", schedule_save)

	client.connect_signal("tagged", schedule_save)
	client.connect_signal("untagged", schedule_save)
	client.connect_signal("property::screen", schedule_save)
	client.connect_signal("property::minimized", schedule_save)
	client.connect_signal("property::fullscreen", schedule_save)
	client.connect_signal("property::maximized", schedule_save)
	client.connect_signal("property::floating", schedule_save)
	client.connect_signal("manage", schedule_save)
	client.connect_signal("unmanage", schedule_save)

	screen.connect_signal("property::geometry", schedule_save)
end

return M
