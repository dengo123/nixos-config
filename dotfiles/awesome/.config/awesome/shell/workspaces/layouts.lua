-- ~/.config/awesome/features/workspaces/layouts.lua
local awful = require("awful")

local M = {}

M.list = {
	awful.layout.suit.max,
	awful.layout.suit.fair,
	awful.layout.suit.fair.horizontal,
}

function M.apply()
	awful.layout.layouts = M.list
end

-- pro Screen erlaubte Layouts
function M.allowed_for(s)
	local g = s.geometry
	if g.width >= g.height then
		return { awful.layout.suit.max, awful.layout.suit.fair }
	else
		return { awful.layout.suit.max, awful.layout.suit.fair.horizontal }
	end
end

-- nächstes Layout innerhalb allowed-Liste
local function next_in_allowed(t, allowed, dir)
	dir = dir or 1
	local cur = t.layout
	local idx = 1
	for i, L in ipairs(allowed) do
		if L == cur then
			idx = i
			break
		end
	end
	local n = #allowed
	local j = ((idx - 1 + dir) % n) + 1
	t.layout = allowed[j]
end

function M.next_on_screen(s)
	s = s or awful.screen.focused()
	local t = s and s.selected_tag
	if not t then
		return
	end
	local allowed = M.allowed_for(s)
	next_in_allowed(t, allowed, 1)
end

function M.prev_on_screen(s)
	s = s or awful.screen.focused()
	local t = s and s.selected_tag
	if not t then
		return
	end
	local allowed = M.allowed_for(s)
	next_in_allowed(t, allowed, -1)
end

-- Toggle: wenn nicht max → max; sonst zum „anderen“ (fair / fair.h)
function M.toggle_to_max(s)
	s = s or awful.screen.focused()
	local t = s and s.selected_tag
	if not t then
		return
	end
	local allowed = M.allowed_for(s)
	if t.layout ~= allowed[1] then
		t.layout = allowed[1] -- always max
	else
		t.layout = allowed[2] or allowed[1]
	end
end

return M
