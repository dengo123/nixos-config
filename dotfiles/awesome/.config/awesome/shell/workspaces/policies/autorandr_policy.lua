-- ~/.config/awesome/shell/workspaces/policies/autorandr_policy.lua
local awful = require("awful")
local gears = require("gears")

local M = {}

-- ========= helpers =========

-- Liefert stabilen Key für einen Screen (xrandr output name)
local function screen_key(s)
	-- Awesome 4.3+ hat oft s.outputs als table: { ["DP-4"]=true, ["HDMI-0"]=true, ... }
	if s and type(s.outputs) == "table" then
		for name, v in pairs(s.outputs) do
			if v then
				return name
			end
		end
	end
	-- Fallback: index (nicht ideal, aber besser als nix)
	return s and tostring(s.index) or "?"
end

local function screen_by_key(key)
	if not key then
		return nil
	end
	for s in screen do
		if screen_key(s) == key then
			return s
		end
	end
	return nil
end

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

-- ========= snapshot =========

M._snap = {
	screens = {}, -- [screen_key] = { selected_tag_idx = number }
	clients = {}, -- [client.window] = { screen_key=string, tag_idx=number, ... }
	ts = 0,
}

local function snapshot()
	local snap = { screens = {}, clients = {}, ts = os.time() }

	-- Screens: Tag-Auswahl merken (pro Output-Key)
	for s in screen do
		local key = screen_key(s)
		snap.screens[key] = {
			selected_tag_idx = safe_selected_tag_idx(s),
		}
	end

	-- Clients: Zuordnung merken
	for _, c in ipairs(client.get()) do
		local s = c.screen
		snap.clients[c.window] = {
			screen_key = s and screen_key(s) or nil,
			tag_idx = client_tag_idx(c),

			minimized = c.minimized or false,
			floating = awful.client.floating.get(c) or false,
			fullscreen = c.fullscreen or false,
			maximized = c.maximized or false,
		}
	end

	M._snap = snap
end

-- ========= restore =========

local function restore_tags()
	for key, ss in pairs(M._snap.screens or {}) do
		local s = screen_by_key(key)
		if s and ss and ss.selected_tag_idx then
			local t = safe_get_tag(s, ss.selected_tag_idx)
			if t then
				t:view_only()
			end
		end
	end
end

local function restore_clients()
	for _, c in ipairs(client.get()) do
		local cs = M._snap.clients[c.window]
		if cs then
			-- Screen restore (über Output-Key!)
			local target_screen = cs.screen_key and screen_by_key(cs.screen_key) or nil
			if target_screen and c.screen ~= target_screen then
				c.screen = target_screen
			end

			-- Tag restore
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

local function restore()
	restore_tags()
	restore_clients()

	gears.timer.start_new(0.25, function()
		restore_tags()
		restore_clients()
		return false
	end)

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
