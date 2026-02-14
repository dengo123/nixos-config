-- ~/.config/awesome/shell/workspaces/policies/autorandr_policy.lua
local awful = require("awful")
local gears = require("gears")

local M = {}

M._snap = {
	screens = {}, -- [screen.index] = { selected_tag_name=string, selected_tag_idx=number }
	clients = {}, -- [client.window] = { screen_idx, tag_name, tag_idx, ... }
	ts = 0,
}

-- ===== helpers =====

local function selected_tag(s)
	return s and s.selected_tag or nil
end

local function safe_selected_tag_name(s)
	local t = selected_tag(s)
	return t and t.name or nil
end

local function safe_selected_tag_idx(s)
	local t = selected_tag(s)
	return t and t.index or nil
end

local function safe_get_tag_by_name(s, name)
	if not s or not s.tags or not name then
		return nil
	end
	for _, t in ipairs(s.tags) do
		if t and t.name == name then
			return t
		end
	end
	return nil
end

local function safe_get_tag_by_idx(s, idx)
	if not s or not s.tags or not idx then
		return nil
	end
	return s.tags[idx]
end

local function client_first_tag(c)
	return c and c.first_tag or nil
end

local function client_tag_name(c)
	local t = client_first_tag(c)
	return t and t.name or nil
end

local function client_tag_idx(c)
	local t = client_first_tag(c)
	return t and t.index or nil
end

local function with_ws_sync_guard(fn)
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

-- ===== snapshot =====

local function snapshot()
	local snap = { screens = {}, clients = {}, ts = os.time() }

	for s in screen do
		snap.screens[s.index] = {
			selected_tag_name = safe_selected_tag_name(s),
			selected_tag_idx = safe_selected_tag_idx(s),
		}
	end

	for _, c in ipairs(client.get()) do
		local sidx = c.screen and c.screen.index or nil
		snap.clients[c.window] = {
			screen_idx = sidx,
			tag_name = client_tag_name(c),
			tag_idx = client_tag_idx(c),

			minimized = c.minimized or false,
			floating = awful.client.floating.get(c) or false,
			fullscreen = c.fullscreen or false,
			maximized = c.maximized or false,
		}
	end

	M._snap = snap
end

-- ===== restore =====

local function restore_tags()
	with_ws_sync_guard(function()
		local primary = awful.screen.primary

		local function apply_for_screen(s)
			local ss = M._snap.screens[s.index]
			if not ss then
				return
			end

			local t = ss.selected_tag_name and safe_get_tag_by_name(s, ss.selected_tag_name) or nil
			if not t and ss.selected_tag_idx then
				t = safe_get_tag_by_idx(s, ss.selected_tag_idx)
			end

			if t and s.selected_tag ~= t then
				t:view_only()
			end
		end

		-- non-primary zuerst
		for s in screen do
			if s ~= primary then
				apply_for_screen(s)
			end
		end

		-- primary zuletzt
		if primary then
			apply_for_screen(primary)
		end
	end)
end

local function restore_clients()
	for _, c in ipairs(client.get()) do
		local cs = M._snap.clients[c.window]
		if cs then
			-- screen restore
			local target = cs.screen_idx and screen[cs.screen_idx] or nil
			if target and target.valid and c.screen ~= target then
				c.screen = target
			end

			-- tag restore (on possibly new screen)
			if c.screen then
				local t = cs.tag_name and safe_get_tag_by_name(c.screen, cs.tag_name) or nil
				if not t and cs.tag_idx then
					t = safe_get_tag_by_idx(c.screen, cs.tag_idx)
				end
				if t then
					c:move_to_tag(t)
				end
			end

			-- light state restore (like your old working version)
			if cs.minimized then
				c.minimized = true
			end

			-- optional (disabled by default, can be re-enabled)
			-- awful.client.floating.set(c, cs.floating)
			-- c.maximized = cs.maximized
			-- c.fullscreen = cs.fullscreen
		end
	end
end

local function restore_pass()
	restore_tags()
	restore_clients()
end

local function restore()
	restore_pass()

	gears.timer.start_new(0.25, function()
		restore_pass()
		return false
	end)

	gears.timer.start_new(0.85, function()
		restore_pass()
		return false
	end)

	awesome.emit_signal("ui::wallpaper_refresh")
end

function M.attach_policy_signals()
	awesome.connect_signal("autorandr::pre", snapshot)
	awesome.connect_signal("autorandr::applied", restore)
end

return M
