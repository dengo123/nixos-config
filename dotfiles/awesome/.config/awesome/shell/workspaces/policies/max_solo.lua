-- ~/.config/awesome/shell/workspaces/policies/max_solo.lua
local gears = require("gears")

local M = {}

local runtime = {
	ctx = {},
	signals_ready = false,
	primary_by_tag = setmetatable({}, { __mode = "k" }),
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function ctx()
	return runtime.ctx or {}
end

local function minimized_api()
	local c = ctx()

	return (c.services and c.services.minimized)
		or (c.external and c.external.minimized)
		or (c.api and c.api.minimized)
		or nil
end

local function current_tag(s)
	return s and s.selected_tag or nil
end

local function client_on_tag(c, t)
	if not (c and c.valid and t) then
		return false
	end

	for _, tc in ipairs(c:tags() or {}) do
		if tc == t then
			return true
		end
	end

	return false
end

local function tag_clients(t, include_minimized)
	if not t then
		return {}
	end

	local s = t.screen
	local out = {}

	for _, c in ipairs(t:clients() or {}) do
		if c.valid and not c.skip_taskbar and c.screen == s then
			if include_minimized or not c.minimized then
				table.insert(out, c)
			end
		end
	end

	return out
end

local function remove_from_minimized(c)
	local Minimized = minimized_api()
	if Minimized and type(Minimized.remove) == "function" then
		Minimized.remove(c)
	end
end

local function get_primary(t)
	return runtime.primary_by_tag[t]
end

local function set_primary(t, c)
	if not t then
		return
	end

	if c and c.valid then
		runtime.primary_by_tag[t] = c
	else
		runtime.primary_by_tag[t] = nil
	end
end

local function is_primary_candidate(c, t)
	return c and c.valid and not c.minimized and not c.floating and client_on_tag(c, t) and c.screen == t.screen
end

local function resolve_primary(t, focused)
	if is_primary_candidate(focused, t) then
		set_primary(t, focused)
		return focused
	end

	local remembered = get_primary(t)
	if is_primary_candidate(remembered, t) then
		return remembered
	end

	for _, c in ipairs(tag_clients(t, true)) do
		if c.valid and not c.floating then
			set_primary(t, c)
			return c
		end
	end

	return nil
end

local function minimize_client(c)
	if not (c and c.valid) then
		return
	end

	if not c.minimized then
		awesome.emit_signal("ui::suppress_center", 0.2)
		c.minimized = true
	end
end

local function unminimize_client(c)
	if not (c and c.valid) then
		return
	end

	if c.minimized then
		c.minimized = false
	end

	remove_from_minimized(c)
end

local function relevant_tag_for_client(c)
	if not (c and c.valid) then
		return nil
	end

	local s = c.screen
	if not s then
		return nil
	end

	local t = s.selected_tag
	if t and client_on_tag(c, t) then
		return t
	end

	return c.first_tag
end

-- =========================================================================
-- Core
-- =========================================================================

function M.apply_for_tag(t, focused, is_max_layout_fn)
	if not (t and t.screen and type(is_max_layout_fn) == "function" and is_max_layout_fn(t.screen)) then
		return
	end

	local keep = {}

	local primary = resolve_primary(t, focused)
	if primary and primary.valid then
		keep[primary] = true
	end

	if focused and focused.valid and client_on_tag(focused, t) then
		keep[focused] = true
	end

	for _, c in ipairs(tag_clients(t, true)) do
		if keep[c] then
			unminimize_client(c)
		else
			minimize_client(c)
		end
	end

	if focused and focused.valid and keep[focused] then
		focused:raise()
	elseif primary and primary.valid then
		primary:raise()
	end
end

function M.apply_from_screen(s, is_max_layout_fn)
	local t = current_tag(s)
	if not t then
		return
	end

	M.apply_for_tag(t, client.focus, is_max_layout_fn)
end

function M.apply_from_focus(c, is_max_layout_fn)
	local t = relevant_tag_for_client(c)
	if not t then
		return
	end

	M.apply_for_tag(t, c, is_max_layout_fn)
end

-- =========================================================================
-- Signals
-- =========================================================================

function M.init_signals(args)
	if runtime.signals_ready then
		return
	end

	args = args or {}
	runtime.ctx = args.ctx or args or {}
	local is_max_layout_fn = args.is_max_layout

	runtime.signals_ready = true

	client.connect_signal("focus", function(c)
		if not (c and c.valid) then
			return
		end

		gears.timer.delayed_call(function()
			M.apply_from_focus(c, is_max_layout_fn)
		end)
	end)

	client.connect_signal("manage", function(c)
		gears.timer.delayed_call(function()
			M.apply_from_focus(c, is_max_layout_fn)
		end)
	end)

	client.connect_signal("property::floating", function(c)
		gears.timer.delayed_call(function()
			M.apply_from_focus(c, is_max_layout_fn)
		end)
	end)

	client.connect_signal("unmanage", function(c)
		local t = relevant_tag_for_client(c)
		if t and get_primary(t) == c then
			set_primary(t, nil)
		end

		gears.timer.delayed_call(function()
			M.apply_from_screen(c and c.screen or nil, is_max_layout_fn)
		end)
	end)

	tag.connect_signal("property::layout", function(t)
		if not (t and t.screen and t.selected) then
			return
		end

		gears.timer.delayed_call(function()
			M.apply_from_screen(t.screen, is_max_layout_fn)
		end)
	end)

	tag.connect_signal("property::selected", function(t)
		if not (t and t.screen and t.selected) then
			return
		end

		gears.timer.delayed_call(function()
			M.apply_from_screen(t.screen, is_max_layout_fn)
		end)
	end)
end

return M
