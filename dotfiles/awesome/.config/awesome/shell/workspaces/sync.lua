-- ~/.config/awesome/shell/workspaces/sync.lua
local awful = require("awful")
local core = require("shell.workspaces.core")

local S = {}

local function each_screen(fn)
	for s in screen do
		fn(s)
	end
end
local function cur_idx(s)
	s = s or awful.screen.focused()
	return (s.selected_tag and s.selected_tag.index) or 1
end
local function ensure_idx(s, idx)
	while #s.tags < idx do
		core.add_silent(s)
	end
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

function S.add()
	local sf = awful.screen.focused()
	local idx = #sf.tags + 1
	each_screen(function(s)
		ensure_idx(s, idx)
	end)
	each_screen(function(s)
		view_idx(s, idx)
	end)
	return sf.tags[idx]
end

function S.add_silent()
	local sf = awful.screen.focused()
	local idx = #sf.tags + 1
	each_screen(function(s)
		ensure_idx(s, idx)
	end)
	return sf.tags[idx]
end

function S.delete_current()
	local sf = awful.screen.focused()
	local idx = cur_idx(sf)
	local next_idx = math.max(1, idx - 1)
	each_screen(function(s)
		if s.tags[idx] then
			view_idx(s, idx)
			core.delete_current(s)
		end
	end)
	each_screen(function(s)
		ensure_idx(s, next_idx)
	end)
	each_screen(function(s)
		view_idx(s, next_idx)
	end)
end

function S.delete_current_force()
	local sf = awful.screen.focused()
	local idx = cur_idx(sf)
	local next_idx = math.max(1, idx - 1)
	each_screen(function(s)
		if s.tags[idx] then
			view_idx(s, idx)
			core.delete_current_force(s)
		end
	end)
	each_screen(function(s)
		ensure_idx(s, next_idx)
	end)
	each_screen(function(s)
		view_idx(s, next_idx)
	end)
end

function S.view_tag_idx(delta)
	delta = delta or 0
	local sf = awful.screen.focused()
	local target = math.max(1, math.min(cur_idx(sf) + delta, max_idx()))
	each_screen(function(s)
		ensure_idx(s, target)
	end)
	each_screen(function(s)
		view_idx(s, target)
	end)
end

function S.view_tag_abs(idx)
	if not idx or idx < 1 then
		return
	end
	each_screen(function(s)
		ensure_idx(s, idx)
	end)
	each_screen(function(s)
		view_idx(s, idx)
	end)
end

return S
