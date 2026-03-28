-- ~/.config/awesome/shell/workspaces/runtime/controller.lua
local awful = require("awful")

local M = {}

local hooks = {
	kill_clients_in_tag = function(_) end,
	apply_layout_policy = function(_) end,
}

local cfg = {
	fixed_count = 9,
	default_layout = awful.layout.suit.max,
}

-- =========================================================================
-- Hooks
-- =========================================================================

function M.set_hooks(value)
	if type(value) ~= "table" then
		return
	end

	for key, fn in pairs(value) do
		if hooks[key] and type(fn) == "function" then
			hooks[key] = fn
		end
	end
end

-- =========================================================================
-- Config
-- =========================================================================

function M.set_config(value)
	value = value or {}

	if value.tags_fixed_count then
		cfg.fixed_count = tonumber(value.tags_fixed_count) or 9
	end

	if value.default_layout then
		cfg.default_layout = value.default_layout
	end
end

-- =========================================================================
-- Internal
-- =========================================================================

local function desired_layout_for(_)
	return cfg.default_layout
end

local function renumber_tags(s)
	for i, t in ipairs(s.tags or {}) do
		local want = tostring(i)

		if t.name ~= want then
			t.name = want
		end
	end
end

local function ensure_fixed_tags(s)
	local count = cfg.fixed_count or 9

	while #s.tags < count do
		local name = tostring(#s.tags + 1)

		awful.tag.add(name, {
			screen = s,
			layout = desired_layout_for(s),
			selected = false,
		})
	end

	renumber_tags(s)

	if not s.selected_tag and s.tags[1] then
		s.tags[1]:view_only()
	end
end

local function fallback_tag_for(s, t)
	if #s.tags == 0 then
		return nil
	end

	local idx = t and t.index or 1
	local next_idx = idx > 1 and idx - 1 or 1

	return s.tags[next_idx] or s.tags[1]
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.ensure(s)
	s = s or awful.screen.focused()
	ensure_fixed_tags(s)
end

function M.renumber(s)
	s = s or awful.screen.focused()
	renumber_tags(s)
end

function M.add(s)
	s = s or awful.screen.focused()

	ensure_fixed_tags(s)

	local cur = s.selected_tag and s.selected_tag.index or 1
	local next_idx = (cur % #s.tags) + 1
	local t = s.tags[next_idx]

	if awful.screen.focused() ~= s then
		awful.screen.focus(s)
	end

	if t then
		t:view_only()
	end

	return t
end

function M.add_silent(s)
	s = s or awful.screen.focused()
	ensure_fixed_tags(s)
	return s.selected_tag
end

function M.delete_current(s)
	s = s or awful.screen.focused()

	local t = s.selected_tag
	if not t then
		return
	end

	if #t:clients() > 0 then
		hooks.kill_clients_in_tag(t, false)
	end

	local fb = fallback_tag_for(s, t)

	if fb then
		fb:view_only()
	end

	renumber_tags(s)
	hooks.apply_layout_policy(s)
end

function M.delete_current_force(s)
	s = s or awful.screen.focused()

	local t = s.selected_tag
	if not t then
		return
	end

	if #t:clients() > 0 then
		hooks.kill_clients_in_tag(t, true)
	end

	local fb = fallback_tag_for(s, t)

	if fb then
		fb:view_only()
	end

	renumber_tags(s)
	hooks.apply_layout_policy(s)
end

function M.ensure_fixed_tags(s)
	ensure_fixed_tags(s)
end

return M
