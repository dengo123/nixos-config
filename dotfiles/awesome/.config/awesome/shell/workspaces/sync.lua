-- ~/.config/awesome/shell/workspaces/sync.lua
local awful = require("awful")
local core = require("shell.workspaces.core")

local S = {}

-- ===== Helpers ================================================================

local function each_screen(fn)
	for s in screen do
		fn(s)
	end
end

local function cur_idx(s)
	s = s or awful.screen.focused()
	return (s.selected_tag and s.selected_tag.index) or 1
end

local function ensure_idx(s, idx)
	while #s.tags < idx do
		core.add_silent(s) -- erstellt fehlende Tags ohne Fokuswechsel
	end
end

local function view_idx(s, idx)
	local t = s.tags[idx]
	if t and t ~= s.selected_tag then
		t:view_only()
	end
end

local function max_idx()
	local m = 0
	each_screen(function(s)
		if #s.tags > m then
			m = #s.tags
		end
	end)
	return m
end

-- Globales Busy-Flag (nicht an `awesome` hängen; awesome ist userdata in 4.3)
local function with_sync_guard(fn)
	_G.WS_SYNC_BUSY = (_G.WS_SYNC_BUSY or 0) + 1
	local ok, err = pcall(fn)
	_G.WS_SYNC_BUSY = (_G.WS_SYNC_BUSY or 1) - 1
	if _G.WS_SYNC_BUSY <= 0 then
		_G.WS_SYNC_BUSY = nil
	end
	if not ok then
		error(err)
	end
end

-- ===== Sync-Aktionen ==========================================================

-- Neuer Tag auf ALLEN Screens erstellen und überall dorthin wechseln
function S.add()
	local sf = awful.screen.focused()
	local idx = #sf.tags + 1

	with_sync_guard(function()
		each_screen(function(s)
			ensure_idx(s, idx)
		end)
		each_screen(function(s)
			view_idx(s, idx)
		end)
	end)

	return sf.tags[idx]
end

-- Neuer Tag auf ALLEN Screens erstellen, Auswahl beibehalten
function S.add_silent()
	local sf = awful.screen.focused()
	local idx = #sf.tags + 1

	with_sync_guard(function()
		each_screen(function(s)
			ensure_idx(s, idx)
		end)
	end)

	return sf.tags[idx]
end

-- Aktuellen Index auf ALLEN Screens löschen (soft) und auf idx-1 wechseln
function S.delete_current()
	local sf = awful.screen.focused()
	local idx = cur_idx(sf)
	local next_idx = math.max(1, idx - 1)

	with_sync_guard(function()
		each_screen(function(s)
			if s.tags[idx] then
				view_idx(s, idx)
				core.delete_current(s) -- respektiert deine client_policy (soft)
			end
		end)

		each_screen(function(s)
			ensure_idx(s, next_idx)
		end)
		each_screen(function(s)
			view_idx(s, next_idx)
		end)
	end)
end

-- Aktuellen Index auf ALLEN Screens löschen (force) und auf idx-1 wechseln
function S.delete_current_force()
	local sf = awful.screen.focused()
	local idx = cur_idx(sf)
	local next_idx = math.max(1, idx - 1)

	with_sync_guard(function()
		each_screen(function(s)
			if s.tags[idx] then
				view_idx(s, idx)
				core.delete_current_force(s)
			end
		end)

		each_screen(function(s)
			ensure_idx(s, next_idx)
		end)
		each_screen(function(s)
			view_idx(s, next_idx)
		end)
	end)
end

-- Relativen Wechsel (±delta) synchron auf ALLEN Screens
function S.view_tag_idx(delta)
	delta = delta or 0
	local sf = awful.screen.focused()
	local target = cur_idx(sf) + delta
	local maxn = max_idx()
	if maxn == 0 then
		return
	end
	if target < 1 then
		target = 1
	end
	if target > maxn then
		target = maxn
	end

	with_sync_guard(function()
		each_screen(function(s)
			ensure_idx(s, target)
		end)
		each_screen(function(s)
			view_idx(s, target)
		end)
	end)
end

-- Absoluter Wechsel (idx) synchron auf ALLEN Screens
function S.view_tag_abs(idx)
	if not idx or idx < 1 then
		return
	end

	with_sync_guard(function()
		each_screen(function(s)
			ensure_idx(s, idx)
		end)
		each_screen(function(s)
			view_idx(s, idx)
		end)
	end)
end

-- ===== Auswahl-Sync (Taglist-Klicks spiegeln) =================================

local _sync_busy = false
function S.init_selection_sync()
	-- alten Handler entfernen, falls vorhanden
	if S._on_tag_selected then
		pcall(tag.disconnect_signal, "property::selected", S._on_tag_selected)
	end

	S._on_tag_selected = function(t)
		if _sync_busy or not (t and t.selected and t.screen) then
			return
		end
		local idx = t.index

		_sync_busy = true
		with_sync_guard(function()
			each_screen(function(s)
				if s ~= t.screen then
					ensure_idx(s, idx)
					view_idx(s, idx)
				end
			end)
		end)
		_sync_busy = false
	end

	tag.connect_signal("property::selected", S._on_tag_selected)
end

return S
