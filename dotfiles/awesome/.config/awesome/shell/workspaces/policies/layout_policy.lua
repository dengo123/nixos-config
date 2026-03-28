-- ~/.config/awesome/shell/workspaces/policies/layout_policy.lua
local awful = require("awful")

local M = {}

local runtime = {
	workspaces = {},
	cfg = {},
	rotation_handler = nil,
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function workspaces()
	return runtime.workspaces or {}
end

local function cfg()
	return runtime.cfg or {}
end

local function layouts_mod()
	return workspaces().layouts
end

local function mode()
	local Layouts = layouts_mod()
	return (Layouts and type(Layouts.mode) == "function" and Layouts.mode(cfg())) or "tiling"
end

local function is_horizontal_screen(s)
	return s and s.geometry and s.geometry.width >= s.geometry.height
end

local function expand_entry(entry, s)
	local Layouts = layouts_mod()
	local horizontal = is_horizontal_screen(s)

	if type(entry) ~= "string" then
		return entry
	end

	if entry == "fair" then
		return horizontal and awful.layout.suit.fair or awful.layout.suit.fair.horizontal
	end

	if entry == "tile_main" then
		return horizontal and awful.layout.suit.tile or awful.layout.suit.tile.top
	end

	if entry == "tile_cross" then
		return horizontal and awful.layout.suit.tile.left or awful.layout.suit.tile.bottom
	end

	return (Layouts and type(Layouts.resolve) == "function" and Layouts.resolve(entry)) or entry
end

local function allowed_for(s)
	local Layouts = layouts_mod()

	if mode() == "floating" then
		return { awful.layout.suit.floating }
	end

	local include = (Layouts and type(Layouts.resolve_include) == "function" and Layouts.resolve_include(cfg())) or {}
	local out = {}

	for _, entry in ipairs(include) do
		table.insert(out, expand_entry(entry, s))
	end

	return out
end

local function in_allowed(current, allowed)
	for _, layout in ipairs(allowed) do
		if layout == current then
			return true
		end
	end

	return false
end

local function same_layout_list(a, b)
	if a == b then
		return true
	end

	if type(a) ~= "table" or type(b) ~= "table" or #a ~= #b then
		return false
	end

	for i, layout in ipairs(b) do
		if a[i] ~= layout then
			return false
		end
	end

	return true
end

local function set_tag_layouts_for_screen(t, s)
	if not (t and s) then
		return
	end

	local allowed = allowed_for(s)

	if not same_layout_list(t.layouts, allowed) then
		t.layouts = allowed
	end
end

local function normalize_for_screen(layout, s)
	local horizontal = is_horizontal_screen(s)

	if layout == awful.layout.suit.fair and not horizontal then
		return awful.layout.suit.fair.horizontal
	end

	if layout == awful.layout.suit.fair.horizontal and horizontal then
		return awful.layout.suit.fair
	end

	if layout == awful.layout.suit.tile and not horizontal then
		return awful.layout.suit.tile.top
	end

	if layout == awful.layout.suit.tile.top and horizontal then
		return awful.layout.suit.tile
	end

	if layout == awful.layout.suit.tile.left and not horizontal then
		return awful.layout.suit.tile.bottom
	end

	if layout == awful.layout.suit.tile.bottom and horizontal then
		return awful.layout.suit.tile.left
	end

	return layout
end

local function fallback_layout(s)
	local Layouts = layouts_mod()

	if mode() == "floating" then
		return awful.layout.suit.floating
	end

	local default_name = (Layouts and type(Layouts.default_name) == "function" and Layouts.default_name(cfg())) or "max"
	return expand_entry(default_name, s)
end

-- =========================================================================
-- Enforcement
-- =========================================================================

local function enforce_on_tag(t)
	if not t or not t.screen or t._enforce_busy then
		return
	end

	local s = t.screen
	local allowed = allowed_for(s)

	set_tag_layouts_for_screen(t, s)

	local wanted = (mode() == "tiling") and normalize_for_screen(t.layout, s) or t.layout

	if in_allowed(wanted, allowed) and wanted ~= t.layout then
		t._enforce_busy = true
		t.layout = wanted
		t._enforce_busy = nil
		return
	end

	if not in_allowed(t.layout, allowed) then
		t._enforce_busy = true
		t.layout = fallback_layout(s)
		t._enforce_busy = nil
	end
end

local function enforce_all_on_screen(s)
	s = s or awful.screen.focused()

	if not s then
		return
	end

	for _, t in ipairs(s.tags or {}) do
		set_tag_layouts_for_screen(t, s)
		enforce_on_tag(t)
	end
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(args)
	args = args or {}

	runtime.workspaces = args.workspaces or runtime.workspaces
	runtime.cfg = args.cfg or (runtime.workspaces and runtime.workspaces.cfg) or args or {}

	return M
end

function M.apply_layout_policy(s)
	s = s or awful.screen.focused()

	if not s or not s.selected_tag then
		return
	end

	set_tag_layouts_for_screen(s.selected_tag, s)
	enforce_on_tag(s.selected_tag)
end

function M.apply_layout_policy_all(s)
	enforce_all_on_screen(s)
end

function M.on_screen_rotation()
	if runtime.rotation_handler then
		screen.disconnect_signal("property::geometry", runtime.rotation_handler)
	end

	runtime.rotation_handler = function()
		for s in screen do
			enforce_all_on_screen(s)
		end
	end

	screen.connect_signal("property::geometry", runtime.rotation_handler)
end

function M.init_enforcement(args)
	args = args or {}

	runtime.workspaces = args.workspaces or runtime.workspaces
	runtime.cfg = args.cfg or (runtime.workspaces and runtime.workspaces.cfg) or args or {}

	tag.disconnect_signal("property::layout", enforce_on_tag)
	tag.connect_signal("property::layout", enforce_on_tag)

	for s in screen do
		enforce_all_on_screen(s)
	end

	M.on_screen_rotation()
end

function M.allowed_for(s)
	return allowed_for(s)
end

return M
