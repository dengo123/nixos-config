-- ~/.config/awesome/shell/workspaces/policies/session_state.lua
local awful = require("awful")
local gears = require("gears")
local gfs = require("gears.filesystem")

local M = {}

local STATE_PATH = gfs.get_cache_dir() .. "session-state.json"

-- ========= screen keying (OUTPUT-NAME statt screen.index) =========

local function screen_key(s)
	if s and type(s.outputs) == "table" then
		for name, v in pairs(s.outputs) do
			if v then
				return name -- z.B. "DP-4" oder "HDMI-0"
			end
		end
	end
	-- fallback (sollte selten nötig sein)
	return s and ("screen:" .. tostring(s.index)) or "screen:?"
end

local function screen_by_key(key)
	for s in screen do
		if screen_key(s) == key then
			return s
		end
	end
	return nil
end

-- ========= json helpers =========

local function read_json(path)
	local f = io.open(path, "r")
	if not f then
		return nil
	end
	local s = f:read("*a")
	f:close()
	if not s or s == "" then
		return nil
	end
	local ok, data = pcall(gears.json.decode, s)
	return ok and data or nil
end

local function write_json(path, data)
	pcall(function()
		gears.filesystem.make_directories(gfs.get_cache_dir())
	end)
	local ok, s = pcall(gears.json.encode, data)
	if not ok or not s then
		return false
	end
	local f = io.open(path, "w")
	if not f then
		return false
	end
	f:write(s)
	f:close()
	return true
end

-- ========= tags =========

local function ensure_tag(s, name, layout)
	for _, t in ipairs(s.tags or {}) do
		if t.name == name then
			return t
		end
	end
	return awful.tag.add(name, {
		screen = s,
		layout = layout or awful.layout.suit.max,
		selected = false,
	})
end

-- ========= snapshot / restore =========

local function snapshot()
	local data = {
		ts = os.time(),
		screens = {}, -- [screen_key] = { selected_tag="N", tags={"1","2",...} }
		clients = {}, -- list of { wid, screen_key, tag_name }
	}

	-- Screens + Tags
	for s in screen do
		local key = screen_key(s)
		local st = {
			selected_tag = (s.selected_tag and s.selected_tag.name) or "1",
			tags = {},
		}
		for _, t in ipairs(s.tags or {}) do
			table.insert(st.tags, t.name)
		end
		data.screens[key] = st
	end

	-- Clients mapping (Window-ID bleibt bei awesome restart stabil)
	for _, c in ipairs(client.get()) do
		local t = c.first_tag
		table.insert(data.clients, {
			wid = c.window,
			screen_key = c.screen and screen_key(c.screen) or nil,
			tag_name = t and t.name or "1",
		})
	end

	write_json(STATE_PATH, data)
end

local function restore()
	local data = read_json(STATE_PATH)
	if not data or not data.screens then
		return
	end

	-- 1) Tags pro Screen rekonstruieren (anhand screen_key)
	for s in screen do
		local key = screen_key(s)
		local st = data.screens[key]
		if st then
			for _, tagname in ipairs(st.tags or {}) do
				ensure_tag(s, tagname)
			end
			ensure_tag(s, "1")

			local sel = st.selected_tag or "1"
			for _, t in ipairs(s.tags or {}) do
				if t.name == sel then
					t:view_only()
					break
				end
			end
		else
			ensure_tag(s, "1")
			s.tags[1]:view_only()
		end
	end

	-- 2) Clients zurücksortieren (2-pass + delayed)
	local function apply_clients()
		for _, c in ipairs(client.get()) do
			local best = nil
			for _, cs in ipairs(data.clients or {}) do
				if cs.wid == c.window then
					best = cs
					break
				end
			end
			if best then
				-- Screen via OUTPUT key
				if best.screen_key then
					local target = screen_by_key(best.screen_key)
					if target and c.screen ~= target then
						c.screen = target
					end
				end

				-- Tag by name (nach Screen-move!)
				if c.screen then
					local t = ensure_tag(c.screen, best.tag_name or "1")
					if t then
						c:move_to_tag(t)
					end
				end
			end
		end
	end

	apply_clients()
	gears.timer.start_new(0.25, function()
		apply_clients()
		return false
	end)

	awesome.emit_signal("ui::wallpaper_refresh")
end

-- ========= signals =========

function M.attach_policy_signals()
	-- Snapshot bei “echtem exit”
	awesome.connect_signal("exit", function(_)
		snapshot()
	end)

	-- Snapshot/Restore bei “Session-Change” (autorandr / reload / etc.)
	awesome.connect_signal("session::pre_change", function(_)
		snapshot()
	end)

	awesome.connect_signal("session::post_change", function(_)
		-- staged restore
		restore()
	end)
end

function M.restore_on_start()
	gears.timer.start_new(0.15, function()
		restore()
		return false
	end)
end

return M
