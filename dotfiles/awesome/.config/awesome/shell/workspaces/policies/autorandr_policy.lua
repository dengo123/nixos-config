-- ~/.config/awesome/shell/workspaces/policies/autorandr_policy.lua
local awful = require("awful")
local gears = require("gears")

local M = {}

-- Snapshot-State
M._snap = {
	screens = {}, -- [screen.index] = { selected_tag_idx = number }
	clients = {}, -- [client.window] = { screen_idx = number, tag_idx = number, minimized=bool, floating=bool, fullscreen=bool, maximized=bool }
	ts = 0,
}

local function safe_selected_tag_idx(s)
	if not s or not s.selected_tag then
		return nil
	end
	return s.selected_tag.index
end

local function safe_get_tag(s, idx)
	if not s or not s.tags or not idx then
		return nil
	end
	return s.tags[idx]
end

local function client_tag_idx(c)
	-- c.first_tag ist stabiler als c:tags()[1] wenn multi-tagging
	local t = c and c.first_tag
	return t and t.index or nil
end

local function snapshot()
	local snap = { screens = {}, clients = {}, ts = os.time() }

	-- Screens: Tag-Auswahl merken
	for s in screen do
		snap.screens[s.index] = {
			selected_tag_idx = safe_selected_tag_idx(s),
		}
	end

	-- Clients: Zuordnung merken
	for _, c in ipairs(client.get()) do
		-- ignore special windows if you want (optional):
		-- if c.type == "dock" or c.type == "desktop" then goto continue end

		local sidx = c.screen and c.screen.index or nil
		snap.clients[c.window] = {
			screen_idx = sidx,
			tag_idx = client_tag_idx(c),
			minimized = c.minimized or false,
			floating = awful.client.floating.get(c) or false,
			fullscreen = c.fullscreen or false,
			maximized = c.maximized or false,
		}
		-- ::continue::
	end

	M._snap = snap
end

local function restore_tags()
	-- erst Tags setzen (damit danach Clients “Ziel” haben)
	for s in screen do
		local ss = M._snap.screens[s.index]
		if ss and ss.selected_tag_idx then
			local t = safe_get_tag(s, ss.selected_tag_idx)
			if t then
				-- view_only ist besser als s.selected_tag = t (letzteres kann manchmal “leise” scheitern)
				t:view_only()
			end
		end
	end
end

local function restore_clients()
	for _, c in ipairs(client.get()) do
		local cs = M._snap.clients[c.window]
		if cs then
			-- Screen restore
			if cs.screen_idx and screen[cs.screen_idx] and c.screen ~= screen[cs.screen_idx] then
				c.screen = screen[cs.screen_idx]
			end

			-- Tag restore (auf dem ggf. neuen Screen!)
			if c.screen and cs.tag_idx then
				local t = safe_get_tag(c.screen, cs.tag_idx)
				if t then
					c:move_to_tag(t)
				end
			end

			-- optional: State restore (nur wenn du willst; ich halte es bewusst “leicht”)
			-- Minimizing direkt nach RandR kann nerven, daher nur wenn es vorher minimized war:
			if cs.minimized then
				c.minimized = true
			end

			-- Floating/Max/Fullscreen können Layout sprengen – wenn du willst, aktivieren:
			-- awful.client.floating.set(c, cs.floating)
			-- c.maximized = cs.maximized
			-- c.fullscreen = cs.fullscreen
		end
	end
end

local function restore()
	-- RandR braucht oft 1-2 “ticks”, deshalb staged restore:
	restore_tags()
	restore_clients()

	-- ein bisschen später nochmal (häufig nötig bei HDMI/4K + primary changes)
	gears.timer.start_new(0.25, function()
		restore_tags()
		restore_clients()
		return false
	end)

	-- Wallpaper nochmal triggern (dein wallpaper.lua lauscht drauf)
	awesome.emit_signal("ui::wallpaper_refresh")
end

function M.attach_policy_signals()
	awesome.connect_signal("autorandr::pre", function()
		snapshot()
	end)

	awesome.connect_signal("autorandr::applied", function()
		restore()
	end)
end

return M
