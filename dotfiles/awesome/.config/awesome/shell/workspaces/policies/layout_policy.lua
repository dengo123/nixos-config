-- ~/.config/awesome/shell/workspaces/policies/layout_policy.lua
local awful = require("awful")

local P = {}

-- Erlaubte Teilmenge je Screen
local function allowed_for(s)
	local g = s.geometry
	if g.width >= g.height then
		return { awful.layout.suit.max, awful.layout.suit.fair } -- horizontal
	else
		return { awful.layout.suit.max, awful.layout.suit.fair.horizontal } -- vertikal
	end
end

local function in_allowed(cur, allowed)
	for _, L in ipairs(allowed) do
		if L == cur then
			return true
		end
	end
	return false
end

-- fair <-> fair.horizontal normalisieren je Screen-Ausrichtung
local function normalize_for_screen(L, s)
	local horiz = s.geometry.width >= s.geometry.height
	if L == awful.layout.suit.fair and not horiz then
		return awful.layout.suit.fair.horizontal
	elseif L == awful.layout.suit.fair.horizontal and horiz then
		return awful.layout.suit.fair
	end
	return L
end

-- ENFORCEMENT (mit Guard)
function P.enforce_on_tag(t)
	if not t or not t.screen or t._enforce_busy then
		return
	end
	local s = t.screen
	local allowed = allowed_for(s)

	local wanted = normalize_for_screen(t.layout, s)
	if in_allowed(wanted, allowed) and wanted ~= t.layout then
		t._enforce_busy = true
		t.layout = wanted
		t._enforce_busy = nil
		return
	end

	if not in_allowed(t.layout, allowed) then
		t._enforce_busy = true
		t.layout = allowed[1] -- Standard = max
		t._enforce_busy = nil
	end
end

function P.enforce_all_on_screen(s)
	s = s or awful.screen.focused()
	if not s then
		return
	end
	for _, t in ipairs(s.tags or {}) do
		P.enforce_on_tag(t)
	end
end

-- öffentliche Policy-API (für core/init)
function P.apply_layout_policy(s)
	s = s or awful.screen.focused()
	if not s or not s.selected_tag then
		return
	end
	P.enforce_on_tag(s.selected_tag)
end

function P.apply_layout_policy_all(s)
	P.enforce_all_on_screen(s)
end

function P.on_screen_rotation()
	screen.disconnect_signal("property::geometry", P.apply_layout_policy_all)
	screen.connect_signal("property::geometry", P.apply_layout_policy_all)
end

-- Hooks aktivieren
function P.init_enforcement()
	tag.disconnect_signal("property::layout", P.enforce_on_tag)
	tag.connect_signal("property::layout", P.enforce_on_tag)
	-- initial für alle Screens
	for s in screen do
		P.enforce_all_on_screen(s)
	end
	-- bei Rotation/Auslösung erneut
	P.on_screen_rotation()
end

-- optional exportieren (falls wer allowed_for braucht)
P.allowed_for = allowed_for

return P
