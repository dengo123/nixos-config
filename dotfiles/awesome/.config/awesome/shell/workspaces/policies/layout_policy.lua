-- ~/.config/awesome/shell/workspaces/policies/layout_policy.lua
local awful = require("awful")

local P = {}

-- Weak-Key Map: pro Tag den zuletzt gesehenen Layout-Wert merken
local LAST = setmetatable({}, { __mode = "k" }) -- Keys (Tags) werden weak gehalten

-- Erlaubte Teilmenge je Screen (inkl. tile-Varianten)
local function allowed_for(s)
	local g = s.geometry
	if g.width >= g.height then
		-- Landscape: max, fair, tile (Master links)
		return { awful.layout.suit.max, awful.layout.suit.fair, awful.layout.suit.tile }
	else
		-- Portrait: max, fair.horizontal, tile.top (Master oben)
		return { awful.layout.suit.max, awful.layout.suit.fair.horizontal, awful.layout.suit.tile.top }
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

-- fair/tile ↔ fair.horizontal/tile.top an Screen-Ausrichtung anpassen
local function normalize_for_screen(L, s)
	local horiz = s.geometry.width >= s.geometry.height
	-- fair
	if L == awful.layout.suit.fair and not horiz then
		return awful.layout.suit.fair.horizontal
	elseif L == awful.layout.suit.fair.horizontal and horiz then
		return awful.layout.suit.fair
	end
	-- tile
	if L == awful.layout.suit.tile and not horiz then
		return awful.layout.suit.tile.top
	elseif L == awful.layout.suit.tile.top and horiz then
		return awful.layout.suit.tile
	end
	return L
end

-- Bridge: bei Wechsel zur Schwester-Variante (fair↔fair.h, tile↔tile.top) → zu max bridgen,
-- damit beim Durchschalten ein sichtbarer “dritter” Zustand existiert.
local function bridge_sister_to_max(t, s)
	local horiz = s.geometry.width >= s.geometry.height
	local prev = LAST[t]
	local cur = t.layout

	if horiz then
		-- fair -> fair.horizontal → bridge zu max
		if prev == awful.layout.suit.fair and cur == awful.layout.suit.fair.horizontal then
			return awful.layout.suit.max
		end
		-- tile -> tile.top → bridge zu max
		if prev == awful.layout.suit.tile and cur == awful.layout.suit.tile.top then
			return awful.layout.suit.max
		end
	else
		-- fair.horizontal -> fair → bridge zu max
		if prev == awful.layout.suit.fair.horizontal and cur == awful.layout.suit.fair then
			return awful.layout.suit.max
		end
		-- tile.top -> tile → bridge zu max
		if prev == awful.layout.suit.tile.top and cur == awful.layout.suit.tile then
			return awful.layout.suit.max
		end
	end

	return nil
end

-- ENFORCEMENT (mit Guard + Weak-Map)
function P.enforce_on_tag(t)
	if not t or not t.screen or t._enforce_busy then
		return
	end
	local s = t.screen
	local allowed = allowed_for(s)

	-- 1) Bridge prüfen (schwesterliches Layout → max)
	local bridged = bridge_sister_to_max(t, s)
	if bridged then
		t._enforce_busy = true
		t.layout = bridged
		t._enforce_busy = nil
		LAST[t] = t.layout
		return
	end

	-- 2) fair/fair.h UND tile/tile.top normalisieren
	local wanted = normalize_for_screen(t.layout, s)
	if in_allowed(wanted, allowed) and wanted ~= t.layout then
		t._enforce_busy = true
		t.layout = wanted
		t._enforce_busy = nil
		LAST[t] = t.layout
		return
	end

	-- 3) Unerlaubtes Layout => Fallback auf allowed[1] (max)
	if not in_allowed(t.layout, allowed) then
		t._enforce_busy = true
		t.layout = allowed[1]
		t._enforce_busy = nil
	end

	-- 4) Zuletzt gesehenes Layout merken
	LAST[t] = t.layout
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

-- öffentliche Policy-API
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

function P.init_enforcement()
	tag.disconnect_signal("property::layout", P.enforce_on_tag)
	tag.connect_signal("property::layout", P.enforce_on_tag)
	for s in screen do
		P.enforce_all_on_screen(s)
	end
	P.on_screen_rotation()
end

P.allowed_for = allowed_for
return P
