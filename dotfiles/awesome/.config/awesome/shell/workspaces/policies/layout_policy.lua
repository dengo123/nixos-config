-- ~/.config/awesome/shell/workspaces/policies/layout_policy.lua
local awful = require("awful")

local P = {}

-- Weak-Key Map: zuletzt gesehenes Layout pro Tag
local LAST = setmetatable({}, { __mode = "k" })

-- ===== Allowed-Listen je Screen erstellen (NEW) =============================
local function allowed_for(s)
	local g = s.geometry
	if g.width >= g.height then
		-- Landscape: exakte Cycle-Reihenfolge definieren
		return { awful.layout.suit.max, awful.layout.suit.fair, awful.layout.suit.tile }
	else
		-- Portrait
		return { awful.layout.suit.max, awful.layout.suit.fair.horizontal, awful.layout.suit.tile.top }
	end
end

-- NEW: Setze die zugrundeliegende tag.layouts-Liste sauber
local function set_tag_layouts_for_screen(t, s)
	if not (t and s) then
		return
	end
	local allowed = allowed_for(s)

	-- Nur updaten, wenn es sich wirklich ändert (vermeidet Signalschleifen)
	local same = true
	if not t.layouts or #t.layouts ~= #allowed then
		same = false
	else
		for i, l in ipairs(allowed) do
			if t.layouts[i] ~= l then
				same = false
				break
			end
		end
	end
	if not same then
		t.layouts = allowed
	end
end

-- Helper
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

-- Bridge fair↔fair.h bzw. tile↔tile.top → max (optional, darf bleiben)
local function bridge_sister_to_max(t, s)
	local horiz = s.geometry.width >= s.geometry.height
	local prev = LAST[t]
	local cur = t.layout

	if horiz then
		if prev == awful.layout.suit.fair and cur == awful.layout.suit.fair.horizontal then
			return awful.layout.suit.max
		end
		if prev == awful.layout.suit.tile and cur == awful.layout.suit.tile.top then
			return awful.layout.suit.max
		end
	else
		if prev == awful.layout.suit.fair.horizontal and cur == awful.layout.suit.fair then
			return awful.layout.suit.max
		end
		if prev == awful.layout.suit.tile.top and cur == awful.layout.suit.tile then
			return awful.layout.suit.max
		end
	end
	return nil
end

-- ===== Enforcement ==========================================================
function P.enforce_on_tag(t)
	if not t or not t.screen or t._enforce_busy then
		return
	end
	local s = t.screen
	local allowed = allowed_for(s)

	-- (NEW) Stelle sicher, dass der Cycle überhaupt nur über die erlaubten Layouts geht
	set_tag_layouts_for_screen(t, s)

	-- 1) Bridge prüfen
	local bridged = bridge_sister_to_max(t, s)
	if bridged then
		t._enforce_busy = true
		t.layout = bridged
		t._enforce_busy = nil
		LAST[t] = t.layout
		return
	end

	-- 2) fair/fair.h & tile/tile.top normalisieren
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
		-- (NEW) vor dem Enforce die layouts-Liste setzen
		set_tag_layouts_for_screen(t, s)
		P.enforce_on_tag(t)
	end
end

-- Öffentliche API
function P.apply_layout_policy(s)
	s = s or awful.screen.focused()
	if not s or not s.selected_tag then
		return
	end
	set_tag_layouts_for_screen(s.selected_tag, s) -- NEW
	P.enforce_on_tag(s.selected_tag)
end

function P.apply_layout_policy_all(s)
	P.enforce_all_on_screen(s)
end

function P.on_screen_rotation()
	screen.disconnect_signal("property::geometry", P.apply_layout_policy_all)
	screen.connect_signal("property::geometry", function()
		-- (NEW) Bei Rotation alle Tags je Screen aktualisieren + enforce
		for sc in screen do
			P.enforce_all_on_screen(sc)
		end
	end)
end

function P.init_enforcement()
	tag.disconnect_signal("property::layout", P.enforce_on_tag)
	tag.connect_signal("property::layout", P.enforce_on_tag)

	-- (NEW) Wenn Tags neu an Screens kommen / initial da sind
	for s in screen do
		P.enforce_all_on_screen(s)
	end

	P.on_screen_rotation()
end

-- Export
P.allowed_for = allowed_for
return P
