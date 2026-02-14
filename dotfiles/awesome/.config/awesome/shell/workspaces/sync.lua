-- ~/.config/awesome/shell/workspaces/sync.lua
local awful = require("awful")
local core = require("shell.workspaces.core")

local S = {}

-- ===============================================================
-- Helpers
-- ===============================================================

local function each_screen(fn)
	for s in screen do
		fn(s)
	end
end

local function cur_idx(s)
	s = s or awful.screen.focused()
	return (s.selected_tag and s.selected_tag.index) or 1
end

local function view_idx(s, idx)
	local t = s.tags[idx]
	if t and t ~= s.selected_tag then
		t:view_only()
	end
end

local function max_idx()
	local m = 0
	each_screen(function(s)
		if #s.tags > m then
			m = #s.tags
		end
	end)
	return m
end

-- Busy Guard
local _sync_busy = false

local function with_guard(fn)
	if _sync_busy then
		return
	end
	_sync_busy = true
	local ok, err = pcall(fn)
	_sync_busy = false
	if not ok then
		error(err)
	end
end

-- ===============================================================
-- Sync Actions
-- ===============================================================

function S.add()
	with_guard(function()
		each_screen(function(s)
			core.add_silent(s)
		end)

		local idx = cur_idx()
		each_screen(function(s)
			view_idx(s, idx)
		end)
	end)
end

function S.add_silent()
	with_guard(function()
		each_screen(function(s)
			core.add_silent(s)
		end)
	end)
end

function S.delete_current()
	local idx = cur_idx()

	with_guard(function()
		each_screen(function(s)
			if s.tags[idx] then
				awful.screen.focus(s)
				core.delete_current(s)
			end
		end)
	end)
end

function S.delete_current_force()
	local idx = cur_idx()

	with_guard(function()
		each_screen(function(s)
			if s.tags[idx] then
				awful.screen.focus(s)
				core.delete_current_force(s)
			end
		end)
	end)
end

function S.view_tag_idx(delta)
	delta = delta or 0

	local target = cur_idx() + delta
	local maxn = max_idx()

	if target < 1 then
		target = 1
	end
	if target > maxn then
		target = maxn
	end

	with_guard(function()
		each_screen(function(s)
			view_idx(s, target)
		end)
	end)
end

function S.view_tag_abs(idx)
	if not idx or idx < 1 then
		return
	end

	with_guard(function()
		each_screen(function(s)
			view_idx(s, idx)
		end)
	end)
end

-- ===============================================================
-- Selection Mirror (Taglist Klicks)
-- ===============================================================

function S.init_selection_sync()
	if S._on_tag_selected then
		pcall(tag.disconnect_signal, "property::selected", S._on_tag_selected)
	end

	S._on_tag_selected = function(t)
		if _sync_busy or not (t and t.selected and t.screen) then
			return
		end

		local idx = t.index

		with_guard(function()
			each_screen(function(s)
				if s ~= t.screen then
					view_idx(s, idx)
				end
			end)
		end)
	end

	tag.connect_signal("property::selected", S._on_tag_selected)
end

return S
