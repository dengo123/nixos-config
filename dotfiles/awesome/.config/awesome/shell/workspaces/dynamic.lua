-- ~/.config/awesome/shell/workspaces/dynamic.lua
local awful = require("awful")
local gears = require("gears")

local M = {}

-- Wir patchen Funktionen in core.lua (feature style)
local core = nil

-- Hooks kommen aus core über set_hooks indirekt – wir holen sie uns über core, wenn du willst.
-- In deinem Core ist HOOKS lokal; deshalb geben wir hier kill/layout als Parameter in enable() mit.
local HOOKS = {
	kill_clients_in_tag = function(_) end, -- (t, force:boolean|nil)
	apply_layout_policy = function(_) end, -- (s)
}

function M.set_hooks(h)
	if type(h) ~= "table" then
		return
	end
	for k, v in pairs(h) do
		if HOOKS[k] and type(v) == "function" then
			HOOKS[k] = v
		end
	end
end

-- ===== helpers =====

local function desired_layout_for(_)
	return awful.layout.suit.max
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
	local cand = s.tags[idx > 1 and (idx - 1) or (idx + 1)] or s.tags[1]
	if cand == t then
		cand = s.tags[1] or s.tags[#s.tags]
	end
	return cand
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
		HOOKS.apply_layout_policy(s)
		return false
	end

	gears.timer.start_new(0.05, try_delete)
end

-- ===== dynamic implementations =====

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
		HOOKS.kill_clients_in_tag(t, false)
		if #t:clients() == 0 then
			delete_tag_when_empty(t)
		else
			renumber_tags(s)
			HOOKS.apply_layout_policy(s)
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

	HOOKS.kill_clients_in_tag(t, true)
	delete_tag_when_empty(t)
end

-- ===== feature toggle =====
-- enable(core_module, hooks_table_optional)
function M.enable(core_module, hooks)
	core = core_module or require("shell.workspaces.core")
	if hooks then
		M.set_hooks(hooks)
	end

	-- patch core API (feature style)
	core.ensure = M.ensure
	core.renumber = M.renumber
	core.add = M.add
	core.add_silent = M.add_silent
	core.delete_current = M.delete_current
	core.delete_current_force = M.delete_current_force

	return true
end

return M
