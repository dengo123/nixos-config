-- ~/.config/awesome/shell/windowing/behavior/focus.lua
local awful = require("awful")
local gears = require("gears")

local F = {}

local runtime = {
	windowing = {},

	raise_on_mouse = false,
	block_ms = 150,

	center_mouse_enable = false,
	center_mouse_exclude_layouts = {},
	center_mouse_exclude_states = {},

	kbd_recent = false,
	suppress_center = false,
	mouse_by_client = setmetatable({}, { __mode = "k" }),
	last_focused_client = nil,
	geometry_follow_ready = false,
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function windowing()
	return runtime.windowing or {}
end

local function list_to_set(list)
	local set = {}

	for _, v in ipairs(list or {}) do
		if type(v) == "string" and v ~= "" then
			set[v] = true
		end
	end

	return set
end

local function current_layout_name(c)
	if not (c and c.valid and c.screen) then
		return nil
	end

	local layout = awful.layout.get(c.screen)
	if not layout then
		return nil
	end

	return awful.layout.getname(layout)
end

local function client_has_excluded_state(c)
	if not (c and c.valid) then
		return false
	end

	if runtime.center_mouse_exclude_states.fullscreen and c.fullscreen then
		return true
	end

	if runtime.center_mouse_exclude_states.maximized and c.maximized then
		return true
	end

	if runtime.center_mouse_exclude_states.maximized_horizontal and c.maximized_horizontal then
		return true
	end

	if runtime.center_mouse_exclude_states.maximized_vertical and c.maximized_vertical then
		return true
	end

	if runtime.center_mouse_exclude_states.floating and c.floating then
		return true
	end

	return false
end

local function should_restore_mouse(c)
	if not (c and c.valid) then
		return false
	end

	local layout_name = current_layout_name(c)
	if layout_name and runtime.center_mouse_exclude_layouts[layout_name] then
		return false
	end

	if client_has_excluded_state(c) then
		return false
	end

	return true
end

local function should_center_mouse(c)
	if not runtime.center_mouse_enable then
		return false
	end

	return should_restore_mouse(c)
end

local function clamp(v, lo, hi)
	if v < lo then
		return lo
	end

	if v > hi then
		return hi
	end

	return v
end

local function remember_mouse_for_client(c)
	if not (c and c.valid) or c.minimized or not c:isvisible() then
		return
	end

	local g = c:geometry()
	local m = mouse.coords()

	if m.x < g.x or m.x >= (g.x + g.width) or m.y < g.y or m.y >= (g.y + g.height) then
		return
	end

	runtime.mouse_by_client[c] = {
		rx = (m.x - g.x) / math.max(g.width, 1),
		ry = (m.y - g.y) / math.max(g.height, 1),
	}
end

local function move_mouse_to_client(c)
	if not (c and c.valid) or c.minimized or not c:isvisible() then
		return
	end

	gears.timer.delayed_call(function()
		if not (c and c.valid) or c.minimized or not c:isvisible() then
			return
		end

		local g = c:geometry()
		local mem = runtime.mouse_by_client[c]

		if mem then
			local px = g.x + math.floor(g.width * mem.rx)
			local py = g.y + math.floor(g.height * mem.ry)

			mouse.coords({
				x = clamp(px, g.x + 12, g.x + math.max(12, g.width - 12)),
				y = clamp(py, g.y + 12, g.y + math.max(12, g.height - 12)),
			})
			return
		end

		local m = mouse.coords()

		mouse.coords({
			x = clamp(m.x, g.x + 12, g.x + math.max(12, g.width - 12)),
			y = clamp(m.y, g.y + 12, g.y + math.max(12, g.height - 12)),
		})
	end)
end

local function center_mouse_in_client(c)
	if not (c and c.valid) or c.minimized or not c:isvisible() then
		return
	end

	gears.timer.delayed_call(function()
		if not (c and c.valid) or c.minimized or not c:isvisible() then
			return
		end

		local g = c:geometry()

		mouse.coords({
			x = g.x + math.floor(g.width / 2),
			y = g.y + math.floor(g.height / 2),
		})
	end)
end

local function should_follow_geometry(c)
	if not (c and c.valid) then
		return false
	end

	if c ~= client.focus then
		return false
	end

	if c.minimized or not c:isvisible() then
		return false
	end

	if runtime.suppress_center then
		return false
	end

	return should_restore_mouse(c)
end

local function hook_geometry_follow()
	if runtime.geometry_follow_ready then
		return
	end

	client.connect_signal("property::geometry", function(c)
		if not should_follow_geometry(c) then
			return
		end

		move_mouse_to_client(c)
	end)

	client.connect_signal("property::screen", function(c)
		if not should_follow_geometry(c) then
			return
		end

		move_mouse_to_client(c)
	end)

	runtime.geometry_follow_ready = true
end

local function resolve_focus_cfg(args)
	args = args or {}

	local fallback = (windowing().focus_cfg or {})

	return {
		raise_on_mouse = (args.raise_on_mouse ~= nil) and args.raise_on_mouse or fallback.raise_on_mouse,
		block_ms = (args.block_ms ~= nil) and args.block_ms or fallback.block_ms,
		center_mouse = (args.center_mouse ~= nil) and args.center_mouse or fallback.center_mouse,
	}
end

-- =========================================================================
-- Public API
-- =========================================================================

function F.init(args)
	args = args or {}
	runtime.windowing = args.windowing or runtime.windowing

	local focus_cfg = resolve_focus_cfg(args)
	local center_cfg = focus_cfg.center_mouse

	runtime.raise_on_mouse = (focus_cfg.raise_on_mouse == true)
	runtime.block_ms = tonumber(focus_cfg.block_ms) or 150

	if type(center_cfg) == "table" then
		runtime.center_mouse_enable = (center_cfg.enable == true)
		runtime.center_mouse_exclude_layouts = list_to_set(center_cfg.exclude_layouts)
		runtime.center_mouse_exclude_states = list_to_set(center_cfg.exclude_states)
	elseif type(center_cfg) == "boolean" then
		runtime.center_mouse_enable = center_cfg
		runtime.center_mouse_exclude_layouts = {}
		runtime.center_mouse_exclude_states = {}
	else
		runtime.center_mouse_enable = false
		runtime.center_mouse_exclude_layouts = {}
		runtime.center_mouse_exclude_states = {}
	end

	awesome.connect_signal("focus_policy::keyboard_intent", function(ms)
		runtime.kbd_recent = true

		gears.timer.start_new(((ms or runtime.block_ms) / 1000), function()
			runtime.kbd_recent = false
			return false
		end)
	end)

	awesome.connect_signal("ui::suppress_center", function(sec)
		runtime.suppress_center = true

		gears.timer.start_new((sec or 0.15), function()
			runtime.suppress_center = false
			return false
		end)
	end)

	hook_geometry_follow()

	return F
end

function F:on_mouse_enter(c)
	if not (c and c.valid) then
		return
	end

	if not awful.client.focus.filter(c) or c.minimized or not c:isvisible() then
		return
	end

	local prev = client.focus
	if prev and prev ~= c and prev.valid then
		remember_mouse_for_client(prev)
	end

	client.focus = c
	runtime.last_focused_client = c

	if runtime.raise_on_mouse then
		c:raise()
	end
end

function F:on_focus(c)
	if not (c and c.valid) or c.minimized or not c:isvisible() then
		return
	end

	local prev = runtime.last_focused_client
	if prev and prev ~= c and prev.valid then
		remember_mouse_for_client(prev)
	end
	runtime.last_focused_client = c

	if not runtime.kbd_recent or runtime.suppress_center then
		return
	end

	if not should_restore_mouse(c) then
		return
	end

	if should_center_mouse(c) then
		center_mouse_in_client(c)
		return
	end

	move_mouse_to_client(c)
end

return F
