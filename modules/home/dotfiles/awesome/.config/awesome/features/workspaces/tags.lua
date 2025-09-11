-- ~/.config/awesome/features/workspaces/tags.lua
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

-- *** NEU: Policy nach Orientierung (tile vs tile.top)
function M.apply_layout_policy(s)
	s = s or awful.screen.focused()
	local g = s.geometry
	local desired = (g.width >= g.height) and awful.layout.suit.tile or awful.layout.suit.tile.top
	-- auf aktuellen Tag anwenden
	if s.selected_tag and s.selected_tag.layout ~= desired then
		s.selected_tag.layout = desired
	end
end

-- *** NEU: bei Rotation/Geometrieänderung neu anwenden
function M.on_screen_rotation()
	screen.connect_signal("property::geometry", function(s)
		M.apply_layout_policy(s)
	end)
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
	if not t or #s.tags <= 1 then
		return
	end -- letzten Tag nie löschen
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
