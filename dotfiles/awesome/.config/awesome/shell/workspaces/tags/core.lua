-- ~/.config/awesome/features/workspaces/tags/core.lua
local awful = require("awful")
local gears = require("gears")

local M = {}

-- Hooks werden von tags/init.lua gesetzt
local HOOKS = {
	-- (t, force:boolean|nil) -> ()
	kill_clients_in_tag = function(_) end,
	-- (s) -> ()
	apply_layout_policy = function(_) end,
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

-- Wähle einen sinnvollen Fallback-Tag auf Screen s, der NICHT t ist
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

-- Warte, bis Tag leer ist, wechsle auf Fallback, lösche, und säubere
local function delete_tag_when_empty(t)
	if not t or not t.screen then
		return
	end
	local s = t.screen

	local function try_delete()
		if #t:clients() > 0 then
			return true -- timer weiter laufen lassen
		end
		if t.selected then
			local fb = fallback_tag_for(s, t)
			if fb then
				fb:view_only()
			end
		end
		local ok = pcall(function()
			t:delete()
		end)
		ensure_one_tag(s)
		renumber_tags(s)
		HOOKS.apply_layout_policy(s)
		return false -- timer stoppen
	end

	gears.timer.start_new(0.05, try_delete)
end

-- API -----------------------------------------------------------------

function M.ensure(s)
	ensure_one_tag(s)
end

function M.renumber(s)
	renumber_tags(s)
end

-- internes Add: Tag ohne Selektion anlegen, optional exklusiv fokussieren
local function add_impl(s, want_focus)
	s = s or awful.screen.focused()
	local name = tostring(#s.tags + 1)

	-- Tag zunächst unselektiert anlegen
	local t = awful.tag.add(name, {
		screen = s,
		layout = desired_layout_for(s),
		selected = false,
	})
	renumber_tags(s)

	if want_focus then
		-- auf richtigen Screen umschalten
		if awful.screen.focused() ~= s then
			awful.screen.focus(s)
		end
		-- exklusiv nur dieses Tag aktivieren
		t:view_only()
	end

	return t
end

-- neues Tag + Fokus (exklusiv)
function M.add(s)
	return add_impl(s, true)
end

-- neues Tag ohne Fokuswechsel (silent)
function M.add_silent(s)
	return add_impl(s, false)
end

-- Soft delete
function M.delete_current(s)
	s = s or awful.screen.focused()
	local t = s and s.selected_tag
	if not t or #s.tags <= 1 then
		return
	end

	if #t:clients() > 0 then
		HOOKS.kill_clients_in_tag(t, false) -- soft
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

-- Force delete
function M.delete_current_force(s)
	s = s or awful.screen.focused()
	local t = s and s.selected_tag
	if not t or #s.tags <= 1 then
		return
	end

	HOOKS.kill_clients_in_tag(t, true)
	delete_tag_when_empty(t)
end

return M
