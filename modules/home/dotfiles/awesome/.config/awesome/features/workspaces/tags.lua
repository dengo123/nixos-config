-- ~/.config/awesome/features/workspaces/tags.lua
local awful = require("awful")
local gears = require("gears")

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

local function focus_master_of_tag(t)
	if not t then
		return
	end
	gears.timer.delayed_call(function()
		-- Kandidat 1: "Master" des Screens
		local c = awful.client.getmaster(t.screen)

		-- Sicherstellen, dass der Master auch auf diesem Tag liegt
		if not (c and c.valid and c.first_tag == t) then
			-- Kandidat 2: erstes tiled-Fenster
			local tiled = awful.client.tiled(t)
			c = (tiled and tiled[1]) or nil
		end

		-- Fallback: irgendein Client des Tags
		if not (c and c.valid) then
			local clients = t:clients()
			c = clients and clients[1] or nil
		end

		if c and c.valid then
			if c.minimized then
				c.minimized = false
			end

			-- nur wenn Fokus nicht schon im Tag liegt
			local focused = client.focus
			if not (focused and focused.valid and focused.first_tag == t) then
				c:emit_signal("request::activate", "tag_switch_master_focus", { raise = true })
			end
		end
	end)
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

-- Signale: wenn ein Tag ausgewählt wird, Policy anwenden + Master fokussieren
function M.attach_policy_signals()
	tag.connect_signal("property::selected", function(t)
		if t.selected and t.screen then
			M.apply_layout_policy(t.screen)
			focus_master_of_tag(t)
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

-- explizit exportieren für manuelle Nutzung (z. B. in Keybinds)
function M.focus_master_current(s)
	s = s or awful.screen.focused()
	if s and s.selected_tag then
		focus_master_of_tag(s.selected_tag)
	end
end

return M
