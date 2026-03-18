-- ~/.config/awesome/shell/workspaces/runtime/sync.lua
local awful = require("awful")

local Base = require("shell.workspaces.runtime.base")

local M = {}

local sync_busy = false

-- =========================================================================
-- Internal
-- =========================================================================

local function each_screen(fn)
	for s in screen do
		fn(s)
	end
end

local function current_index(s)
	s = s or awful.screen.focused()
	return (s.selected_tag and s.selected_tag.index) or 1
end

local function view_index(s, idx)
	local t = s.tags[idx]

	if t and t ~= s.selected_tag then
		t:view_only()
	end
end

local function max_index()
	local out = 0

	each_screen(function(s)
		if #s.tags > out then
			out = #s.tags
		end
	end)

	return out
end

local function with_guard(fn)
	if sync_busy then
		return
	end

	sync_busy = true

	local ok, err = pcall(fn)

	sync_busy = false

	if not ok then
		error(err)
	end
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.add()
	with_guard(function()
		each_screen(function(s)
			Base.add_silent(s)
		end)

		local idx = current_index()

		each_screen(function(s)
			view_index(s, idx)
		end)
	end)
end

function M.add_silent()
	with_guard(function()
		each_screen(function(s)
			Base.add_silent(s)
		end)
	end)
end

function M.delete_current()
	local idx = current_index()

	with_guard(function()
		each_screen(function(s)
			if s.tags[idx] then
				awful.screen.focus(s)
				Base.delete_current(s)
			end
		end)
	end)
end

function M.delete_current_force()
	local idx = current_index()

	with_guard(function()
		each_screen(function(s)
			if s.tags[idx] then
				awful.screen.focus(s)
				Base.delete_current_force(s)
			end
		end)
	end)
end

function M.view_tag_idx(delta)
	delta = delta or 0

	local target = current_index() + delta
	local maxn = max_index()

	if target < 1 then
		target = 1
	end

	if target > maxn then
		target = maxn
	end

	with_guard(function()
		each_screen(function(s)
			view_index(s, target)
		end)
	end)
end

function M.view_tag_abs(idx)
	if not idx or idx < 1 then
		return
	end

	with_guard(function()
		each_screen(function(s)
			view_index(s, idx)
		end)
	end)
end

function M.init_selection_sync()
	if M._on_tag_selected then
		pcall(tag.disconnect_signal, "property::selected", M._on_tag_selected)
	end

	M._on_tag_selected = function(t)
		if sync_busy or not (t and t.selected and t.screen) then
			return
		end

		local idx = t.index

		with_guard(function()
			each_screen(function(s)
				if s ~= t.screen then
					view_index(s, idx)
				end
			end)
		end)
	end

	tag.connect_signal("property::selected", M._on_tag_selected)
end

return M
