-- ~/.config/awesome/shell/workspaces/policies/layout_policy.lua
local awful = require("awful")

local Layouts = require("shell.workspaces.runtime.layouts")

local M = {}

local last = setmetatable({}, { __mode = "k" })
local runtime_cfg = {}

-- =========================================================================
-- Helpers
-- =========================================================================

local function tags_cfg()
	return runtime_cfg.tags or {}
end

local function layouts_cfg()
	return tags_cfg().layouts or {}
end

local function mode()
	return Layouts.mode(runtime_cfg)
end

local function is_horizontal_screen(s)
	return s.geometry.width >= s.geometry.height
end

local function expand_entry(entry, s)
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
		return horizontal and awful.layout.suit.tile.right or awful.layout.suit.tile.bottom
	end

	if entry == "max" then
		if (not horizontal) and Layouts.use_max_fullscreen_for_portrait(runtime_cfg) then
			return awful.layout.suit.max.fullscreen
		end

		return awful.layout.suit.max
	end

	return Layouts.resolve(entry)
end

local function allowed_for(s)
	if mode() == "floating" then
		return { awful.layout.suit.floating }
	end

	local include = Layouts.resolve_include(runtime_cfg)
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

local function set_tag_layouts_for_screen(t, s)
	if not (t and s) then
		return
	end

	local allowed = allowed_for(s)
	local same = true

	if not t.layouts or #t.layouts ~= #allowed then
		same = false
	else
		for i, layout in ipairs(allowed) do
			if t.layouts[i] ~= layout then
				same = false
				break
			end
		end
	end

	if not same then
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

	if layout == awful.layout.suit.tile.right and not horizontal then
		return awful.layout.suit.tile.bottom
	end

	if layout == awful.layout.suit.tile.bottom and horizontal then
		return awful.layout.suit.tile.right
	end

	if layout == awful.layout.suit.max and not horizontal and Layouts.use_max_fullscreen_for_portrait(runtime_cfg) then
		return awful.layout.suit.max.fullscreen
	end

	if layout == awful.layout.suit.max.fullscreen and horizontal then
		return awful.layout.suit.max
	end

	if layout == awful.layout.suit.max.fullscreen and not Layouts.use_max_fullscreen_for_portrait(runtime_cfg) then
		return awful.layout.suit.max
	end

	return layout
end

local function bridge_sister_to_max(t, s)
	local horizontal = is_horizontal_screen(s)
	local prev = last[t]
	local current = t.layout

	if horizontal then
		if prev == awful.layout.suit.fair and current == awful.layout.suit.fair.horizontal then
			return awful.layout.suit.max
		end

		if prev == awful.layout.suit.tile and current == awful.layout.suit.tile.top then
			return awful.layout.suit.max
		end

		if prev == awful.layout.suit.tile.right and current == awful.layout.suit.tile.bottom then
			return awful.layout.suit.max
		end

		if prev == awful.layout.suit.max and current == awful.layout.suit.max.fullscreen then
			return awful.layout.suit.fair
		end
	else
		if prev == awful.layout.suit.fair.horizontal and current == awful.layout.suit.fair then
			return awful.layout.suit.max
		end

		if prev == awful.layout.suit.tile.top and current == awful.layout.suit.tile then
			return awful.layout.suit.max
		end

		if prev == awful.layout.suit.tile.bottom and current == awful.layout.suit.tile.right then
			return awful.layout.suit.max
		end

		if prev == awful.layout.suit.max.fullscreen and current == awful.layout.suit.max then
			return awful.layout.suit.fair.horizontal
		end
	end

	return nil
end

local function fallback_layout(s)
	if mode() == "floating" then
		return awful.layout.suit.floating
	end

	return expand_entry(Layouts.default_name(runtime_cfg), s)
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

	if mode() == "tiling" then
		local bridged = bridge_sister_to_max(t, s)

		if bridged and in_allowed(bridged, allowed) then
			t._enforce_busy = true
			t.layout = bridged
			t._enforce_busy = nil
			last[t] = t.layout
			return
		end
	end

	local wanted = (mode() == "tiling") and normalize_for_screen(t.layout, s) or t.layout

	if in_allowed(wanted, allowed) and wanted ~= t.layout then
		t._enforce_busy = true
		t.layout = wanted
		t._enforce_busy = nil
		last[t] = t.layout
		return
	end

	if not in_allowed(t.layout, allowed) then
		t._enforce_busy = true
		t.layout = fallback_layout(s)
		t._enforce_busy = nil
	end

	last[t] = t.layout
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
	screen.disconnect_signal("property::geometry", M.apply_layout_policy_all)

	screen.connect_signal("property::geometry", function()
		for s in screen do
			enforce_all_on_screen(s)
		end
	end)
end

function M.init_enforcement(cfg)
	runtime_cfg = cfg or {}

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
