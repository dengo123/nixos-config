-- ~/.config/awesome/shell/workspaces/core.lua
local awful = require("awful")
local gears = require("gears")

local C = {}

-- =====================================================================
-- Hooks (werden von policies/init via workspaces/init gesetzt)
-- =====================================================================

local HOOKS = {
	kill_clients_in_tag = function(_) end, -- (t, force:boolean|nil)
	apply_layout_policy = function(_) end, -- (s)
}

function C.set_hooks(h)
	if type(h) ~= "table" then
		return
	end
	for k, v in pairs(h) do
		if HOOKS[k] and type(v) == "function" then
			HOOKS[k] = v
		end
	end
end

-- =====================================================================
-- Fixed Tag Configuration
-- =====================================================================

C._cfg = {
	fixed_count = 9,
}

function C.set_config(cfg)
	cfg = cfg or {}
	if cfg.tags_fixed_count then
		C._cfg.fixed_count = tonumber(cfg.tags_fixed_count) or 9
	end
end

-- =====================================================================
-- Helpers
-- =====================================================================

local function desired_layout_for(_)
	return awful.layout.suit.max
end

local function renumber_tags(s)
	for i, t in ipairs(s.tags or {}) do
		local want = tostring(i)
		if t.name ~= want then
			t.name = want
		end
	end
end

local function ensure_fixed_tags(s)
	local n = C._cfg.fixed_count or 9

	-- fehlende Tags erzeugen
	while #s.tags < n do
		local name = tostring(#s.tags + 1)
		awful.tag.add(name, {
			screen = s,
			layout = desired_layout_for(s),
			selected = false,
		})
	end

	renumber_tags(s)

	-- Falls kein Tag ausgewählt → Tag 1 aktivieren
	if not s.selected_tag and s.tags[1] then
		s.tags[1]:view_only()
	end
end

local function fallback_tag_for(s, t)
	if #s.tags == 0 then
		return nil
	end
	local idx = t and t.index or 1
	local next_idx = idx > 1 and idx - 1 or 1
	return s.tags[next_idx] or s.tags[1]
end

-- =====================================================================
-- Public API
-- =====================================================================

function C.ensure(s)
	s = s or awful.screen.focused()
	ensure_fixed_tags(s)
end

function C.renumber(s)
	s = s or awful.screen.focused()
	renumber_tags(s)
end

function C.add(s)
	s = s or awful.screen.focused()

	ensure_fixed_tags(s)

	-- fixed-mode: wechsle zum nächsten freien oder nächsten Tag
	local cur = s.selected_tag and s.selected_tag.index or 1
	local next_idx = (cur % #s.tags) + 1
	local t = s.tags[next_idx]

	if awful.screen.focused() ~= s then
		awful.screen.focus(s)
	end

	if t then
		t:view_only()
	end

	return t
end

function C.add_silent(s)
	s = s or awful.screen.focused()
	ensure_fixed_tags(s)
	return s.selected_tag
end

function C.delete_current(s)
	s = s or awful.screen.focused()
	local t = s.selected_tag
	if not t then
		return
	end

	-- fixed-mode: Tag niemals löschen
	if #t:clients() > 0 then
		HOOKS.kill_clients_in_tag(t, false)
	end

	local fb = fallback_tag_for(s, t)
	if fb then
		fb:view_only()
	end

	renumber_tags(s)
	HOOKS.apply_layout_policy(s)
end

function C.delete_current_force(s)
	s = s or awful.screen.focused()
	local t = s.selected_tag
	if not t then
		return
	end

	if #t:clients() > 0 then
		HOOKS.kill_clients_in_tag(t, true)
	end

	local fb = fallback_tag_for(s, t)
	if fb then
		fb:view_only()
	end

	renumber_tags(s)
	HOOKS.apply_layout_policy(s)
end

-- Optional export (für sync)
function C.ensure_fixed_tags(s)
	ensure_fixed_tags(s)
end

return C
