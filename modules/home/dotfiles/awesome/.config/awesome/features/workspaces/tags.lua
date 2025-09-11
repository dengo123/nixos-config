-- ~/.config/awesome/features/workspaces/tags.lua
local awful = require("awful")

local M = {}

-- interne Helfer ------------------------------------------------------

local function renumber_tags(s)
	s = s or awful.screen.focused()
	for i, t in ipairs(s.tags or {}) do
		local want = tostring(i)
		if t.name ~= want then
			t.name = want
		end
	end
end

-- Layout-Auswahl je nach Bildschirm-Orientierung
local function desired_layout_for(s)
	local g = s.geometry
	return (g.width >= g.height) and awful.layout.suit.tile or awful.layout.suit.tile.top
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

-- API -----------------------------------------------------------------

-- Policy auf aktuellen Tag anwenden
function M.apply_layout_policy(s)
	s = s or awful.screen.focused()
	if not s then
		return
	end
	local desired = desired_layout_for(s)
	if s.selected_tag and s.selected_tag.layout ~= desired then
		s.selected_tag.layout = desired
	end
end

-- Policy auf alle Tags des Screens anwenden (z. B. nach Rotation)
function M.apply_layout_policy_all(s)
	s = s or awful.screen.focused()
	if not s then
		return
	end
	local desired = desired_layout_for(s)
	for _, t in ipairs(s.tags or {}) do
		if t.layout ~= desired then
			t.layout = desired
		end
	end
end

-- bei Rotation/Geometrieänderung alle Tags anpassen
function M.on_screen_rotation()
	screen.connect_signal("property::geometry", function(s)
		M.apply_layout_policy_all(s)
	end)
end

-- Signale: wenn ein Tag ausgewählt wird, Policy auf diesen Screen anwenden
function M.attach_policy_signals()
	tag.connect_signal("property::selected", function(t)
		if t.selected and t.screen then
			M.apply_layout_policy(t.screen)
		end
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
		layout = desired_layout_for(s),
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
	-- optional: Layout-Policy erneut anwenden
	M.apply_layout_policy(s)
end

return M
