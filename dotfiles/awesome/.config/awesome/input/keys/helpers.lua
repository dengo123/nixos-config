-- ~/.config/awesome/input/keys/helpers.lua
local awful = require("awful")
local gtable = require("gears.table")

local H = {}

-- ---------- Screen/Tag Utils ----------

function H.scr_in_dir(dir)
	local s = awful.screen.focused()
	return s and s:get_next_in_direction(dir) or nil
end

function H.view_tag_idx(delta, s)
	s = s or awful.screen.focused()
	if not s or not s.selected_tag then
		return
	end
	awful.tag.viewidx(delta, s)
end

-- Verschiebt den AKTUELLEN Tag des aktiven Screens auf Nachbar-Screen
function H.move_tag_to_screen(dir)
	local s = awful.screen.focused()
	if not s or not s.selected_tag then
		return
	end
	local target = H.scr_in_dir(dir)
	if not target then
		return
	end
	local t = s.selected_tag
	t.screen = target
	t:view_only()
	awful.screen.focus(target)
end

-- ---------- Client Move/Swap ----------

function H.move_client_dir(dir)
	local c = client.focus
	if not c then
		return
	end
	awful.client.swap.bydirection(dir, c, nil)
end

-- Client auf Nachbar-Screen verschieben und im selektierten Tag platzieren
function H.move_client_to_screen(dir)
	local c = client.focus
	if not c then
		return
	end
	local target = H.scr_in_dir(dir)
	if not target then
		return
	end

	-- Zieltag wählen (vorzugsweise selected_tag, sonst 1.)
	local t = target.selected_tag or (target.tags and target.tags[1])
	c:move_to_screen(target)
	if t then
		c:move_to_tag(t)
		t:view_only()
	end
	awful.screen.focus(target)
	client.focus = c
	c:raise()
end

-- Client auf Nachbar-Tag verschieben (optional folgen)
function H.move_client_to_neighbor_tag(delta, follow)
	local s = awful.screen.focused()
	if not s or not s.selected_tag then
		return
	end
	local tags = s.tags
	local idx = (s.selected_tag.index or 1) + delta
	local nt = tags[idx]
	local c = client.focus
	if c and nt then
		c:move_to_tag(nt)
		if follow then
			nt:view_only()
			client.focus = c
			c:raise()
		end
	end
end

-- ---------- Pseudo-Maximize (reversibel) ----------

local function save_prev_geom(c)
	if not (c and c.valid) then
		return
	end
	-- Pro Screen getrennt speichern (wichtig bei Multi-Monitor)
	local key = "_prev_geom_s" .. tostring(c.screen.index or 0)
	if not c[key] then
		c[key] = c:geometry()
	end
end

local function pop_prev_geom(c)
	if not (c and c.valid) then
		return
	end
	local key = "_prev_geom_s" .. tostring(c.screen.index or 0)
	local g = c[key]
	c[key] = nil
	return g
end

-- Maximiert ohne maximized-Flags (Floating bleibt nutzbar).
-- Merkt sich vorherige Geometrie pro Screen, um sauber zurückkehren zu können.
function H.pseudo_maximize(c, opts)
	if not (c and c.valid) then
		return
	end
	local defaults = { honor_workarea = true, margins = 0 }
	local merged = gtable.crush({}, defaults, true)
	if opts then
		gtable.crush(merged, opts, true)
	end

	-- Vorbereitung
	save_prev_geom(c)
	c.floating = true -- sicheres Arbeiten mit placement
	c.maximized = false -- echte Max-Flags aus
	c.maximized_horizontal = false
	c.maximized_vertical = false

	-- Platzierung auf Workarea des aktuellen Screens
	awful.placement.maximize(c, merged)
	c.maximized_fake = true
	c:raise()
	return c
end

-- Stellt vorherige Geometrie wieder her (falls vorhanden)
function H.unpseudo_maximize(c)
	if not (c and c.valid) then
		return
	end
	local g = pop_prev_geom(c)
	c.maximized_fake = false
	if g then
		c:geometry(g)
	end
	c:raise()
	return c
end

-- Komfort: toggelt pseudo_maximize ↔ restore
function H.toggle_pseudo_maximize(c, opts)
	if not (c and c.valid) then
		return
	end
	if c.maximized_fake then
		return H.unpseudo_maximize(c)
	else
		return H.pseudo_maximize(c, opts)
	end
end

-- Sicherheit: wenn ein Client den Screen wechselt, alte prev_geom nicht „durchschleppen“
function H.clear_prev_geom_for_screen_change(c)
	if not (c and c.valid) then
		return
	end
	-- einfach alle gespeicherten Keys leeren
	for i = 1, 16 do
		c["_prev_geom_s" .. i] = nil
	end
end

return H
