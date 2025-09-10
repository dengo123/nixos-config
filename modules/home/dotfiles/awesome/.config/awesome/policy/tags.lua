-- ~/.config/awesome/tags.lua
local awful = require("awful")

local M = {}

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
			layout = awful.layout.suit.tile,
			selected = true,
		})
	end
end

function M.ensure(s)
	ensure_one_tag(s)
end

function M.renumber(s)
	renumber_tags(s)
end

function M.add(s)
	s = s or awful.screen.focused()
	local name = tostring(#s.tags + 1)
	local t = awful.tag.add(name, {
		screen = s,
		layout = awful.layout.suit.tile,
		selected = true,
	})
	renumber_tags(s)
	return t
end

function M.delete_current(s)
	s = s or awful.screen.focused()
	local t = s.selected_tag
	if not t then
		return
	end
	if #s.tags <= 1 then
		return
	end -- niemals den letzten Tag killen
	local idx = t.index
	local fallback = s.tags[idx - 1] or s.tags[idx + 1]
	if fallback then
		fallback:view_only()
	end
	t:delete()
	ensure_one_tag(s)
	renumber_tags(s)
end

return M
