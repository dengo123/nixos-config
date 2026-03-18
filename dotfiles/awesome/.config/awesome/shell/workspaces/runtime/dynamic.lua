-- ~/.config/awesome/shell/workspaces/runtime/dynamic.lua
local awful = require("awful")
local gears = require("gears")

local M = {}

local hooks = {
	kill_clients_in_tag = function(_) end,
	apply_layout_policy = function(_) end,
}

local cfg = {
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
	s = s or awful.screen.focused()

	for i, t in ipairs(s.tags or {}) do
		local want = tostring(i)

		if t.name ~= want then
			t.name = want
		end
	end
end

local function ensure_one_tag(s)
	s = s or awful.screen.focused()

	if #s.tags == 0 then
		awful.tag.add("1", {
			screen = s,
			layout = desired_layout_for(s),
			selected = true,
		})
	end
end

local function fallback_tag_for(s, t)
	s = s or awful.screen.focused()

	if not s or #s.tags == 0 then
		return nil
	end

	local idx = t and t.index or (s.selected_tag and s.selected_tag.index) or 1
	local candidate = s.tags[idx > 1 and (idx - 1) or (idx + 1)] or s.tags[1]

	if candidate == t then
		candidate = s.tags[1] or s.tags[#s.tags]
	end

	return candidate
end

local function delete_tag_when_empty(t)
	if not t or not t.screen then
		return
	end

	local s = t.screen

	local function try_delete()
		if #t:clients() > 0 then
			return true
		end

		if t.selected then
			local fb = fallback_tag_for(s, t)

			if fb then
				fb:view_only()
			end
		end

		pcall(function()
			t:delete()
		end)

		ensure_one_tag(s)
		renumber_tags(s)
		hooks.apply_layout_policy(s)

		return false
	end

	gears.timer.start_new(0.05, try_delete)
end

local function add_impl(s, want_focus)
	s = s or awful.screen.focused()

	ensure_one_tag(s)

	local name = tostring(#s.tags + 1)
	local t = awful.tag.add(name, {
		screen = s,
		layout = desired_layout_for(s),
		selected = false,
	})

	renumber_tags(s)

	if want_focus then
		if awful.screen.focused() ~= s then
			awful.screen.focus(s)
		end

		t:view_only()
	end

	return t
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.ensure(s)
	ensure_one_tag(s)
end

function M.renumber(s)
	renumber_tags(s)
end

function M.add(s)
	return add_impl(s, true)
end

function M.add_silent(s)
	return add_impl(s, false)
end

function M.delete_current(s)
	s = s or awful.screen.focused()

	local t = s and s.selected_tag
	if not t or #s.tags <= 1 then
		return
	end

	if #t:clients() > 0 then
		hooks.kill_clients_in_tag(t, false)

		if #t:clients() == 0 then
			delete_tag_when_empty(t)
		else
			renumber_tags(s)
			hooks.apply_layout_policy(s)
		end

		return
	end

	delete_tag_when_empty(t)
end

function M.delete_current_force(s)
	s = s or awful.screen.focused()

	local t = s and s.selected_tag
	if not t or #s.tags <= 1 then
		return
	end

	hooks.kill_clients_in_tag(t, true)
	delete_tag_when_empty(t)
end

function M.enable(base_module, value)
	local base = base_module or require("shell.workspaces.runtime.base")

	if value then
		M.set_hooks(value)
	end

	base.ensure = M.ensure
	base.renumber = M.renumber
	base.add = M.add
	base.add_silent = M.add_silent
	base.delete_current = M.delete_current
	base.delete_current_force = M.delete_current_force

	return true
end

return M
