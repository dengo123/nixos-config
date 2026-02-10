-- ~/.config/awesome/shell/workspaces/policies/autorandr_policy.lua
local awful = require("awful")
local gears = require("gears")

local M = {}

M._snap = { screens = {}, clients = {}, ts = 0 }

local function safe_selected_tag_idx(s)
	return (s and s.selected_tag and s.selected_tag.index) or nil
end

local function safe_get_tag(s, idx)
	if not s or not s.tags or not idx then
		return nil
	end
	return s.tags[idx]
end

local function client_tag_idx(c)
	local t = c and c.first_tag
	return t and t.index or nil
end

local function snapshot()
	local snap = { screens = {}, clients = {}, ts = os.time() }

	for s in screen do
		snap.screens[s.index] = { selected_tag_idx = safe_selected_tag_idx(s) }
	end

	for _, c in ipairs(client.get()) do
		local sidx = c.screen and c.screen.index or nil
		snap.clients[c.window] = {
			screen_idx = sidx,
			tag_idx = client_tag_idx(c),
			minimized = c.minimized or false,
			-- bewusst leicht lassen (wie bei dir)
		}
	end

	M._snap = snap
end

local function restore_tags()
	for s in screen do
		local ss = M._snap.screens[s.index]
		if ss and ss.selected_tag_idx then
			local t = safe_get_tag(s, ss.selected_tag_idx)
			if t and s.selected_tag ~= t then
				t:view_only()
			end
		end
	end
end

local function restore_clients()
	for _, c in ipairs(client.get()) do
		local cs = M._snap.clients[c.window]
		if cs then
			-- Screen restore (nur wenn target existiert)
			local target = cs.screen_idx and screen[cs.screen_idx] or nil
			if target and target.valid and c.screen ~= target then
				c.screen = target
			end

			-- Tag restore (auf dem ggf. neuen Screen!)
			if c.screen and cs.tag_idx then
				local t = safe_get_tag(c.screen, cs.tag_idx)
				if t then
					c:move_to_tag(t)
				end
			end

			if cs.minimized then
				c.minimized = true
			end
		end
	end
end

local function restore_pass()
	restore_tags()
	restore_clients()
end

local function restore()
	restore_pass()

	-- staged: HDMI/4K/primary changes brauchen oft mehrere ticks
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
