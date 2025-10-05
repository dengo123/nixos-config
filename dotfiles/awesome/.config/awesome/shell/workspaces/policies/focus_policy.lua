-- ~/.config/awesome/shell/workspaces/policies/focus_policy.lua
local awful = require("awful")
local gears = require("gears")

local M = {}

-- Für jeden Tag merken wir:
--   last_any      -> zuletzt fokussierter Client (screen-agnostisch)
--   by_screen[s]  -> zuletzt fokussierter Client auf Screen s
--
-- Keys (Tags/Clients/Screens) werden weak gehalten, damit nichts festgeklemmt wird.
local LAST_FOCUS = setmetatable({}, { __mode = "k" }) -- tag -> record

local function ensure_record_for_tag(t)
	local r = LAST_FOCUS[t]
	if not r then
		r = {
			last_any = nil,
			by_screen = setmetatable({}, { __mode = "k" }), -- screen -> client
		}
		LAST_FOCUS[t] = r
	end
	return r
end

local function set_last_focus(tag, screen, client)
	if not (tag and screen and client and client.valid) then
		return
	end
	local r = ensure_record_for_tag(tag)
	r.last_any = client
	r.by_screen[screen] = client
end

local function get_last_focus_for(tag, screen)
	local r = LAST_FOCUS[tag]
	if not r then
		return nil
	end
	local c = r.by_screen[screen]
	if c and c.valid then
		return c
	end
	c = r.last_any
	if c and c.valid then
		return c
	end
	return nil
end

-- Maus beim Tagwechsel nicht „mitziehen“ + Zentrier-Unterdrückung
local function with_mouse_lock(fn)
	local pos = mouse.coords()
	awesome.emit_signal("ui::suppress_center", 0.25)
	fn()
	gears.timer.delayed_call(function()
		if pos and pos.x and pos.y then
			mouse.coords(pos, true) -- true: keine Extra-Events
		end
	end)
end

-- Kandidatenauswahl: priorisiere "per Screen gemerkt" -> first tiled -> irgendein Client
local function focus_last_of_tag_on_screen_or_fallback(tag, screen)
	if not (tag and screen) then
		return
	end

	gears.timer.delayed_call(function()
		if awful.screen.focused() ~= screen then
			awful.screen.focus(screen) -- Screen-Fokus hart setzen
		end

		local function on_tag_screen(c)
			return c and c.valid and c.screen == screen and c.first_tag == tag
		end

		-- 0) der per Screen gemerkte bzw. last_any
		local c = get_last_focus_for(tag, screen)
		if not on_tag_screen(c) then
			c = nil
		end

		-- 1) erster tiled-Client auf DIESEM Screen/Tag
		if not c then
			local tiled = awful.client.tiled(tag)
			if tiled then
				for _, x in ipairs(tiled) do
					if on_tag_screen(x) then
						c = x
						break
					end
				end
			end
		end

		-- 2) irgendein Client dieses Tags auf DIESEM Screen
		if not c then
			local list = tag:clients()
			if list then
				for _, x in ipairs(list) do
					if on_tag_screen(x) then
						c = x
						break
					end
				end
			end
		end

		if c and c.valid then
			if c.minimized then
				c.minimized = false
			end
			if awful.screen.focused() ~= screen then
				awful.screen.focus(screen)
			end
			c:emit_signal("request::activate", "tag_switch_last_focus_per_screen", { raise = true })

			-- Nachkontrolle: bleib bitte auf unserem Screen
			gears.timer.delayed_call(function()
				if client.focus and client.focus.valid and client.focus.screen ~= screen then
					awful.screen.focus(screen)
					c:emit_signal("request::activate", "tag_switch_enforce_screen", { raise = true })
				end
			end)
		end
	end)
end

-- ==== Lernen/Aufräumen der "letzten Fokus"-Infos ======================

client.connect_signal("focus", function(c)
	if not (c and c.valid) then
		return
	end
	local t, s = c.first_tag, c.screen
	if t and s then
		set_last_focus(t, s, c)
	end
end)

client.connect_signal("tagged", function(c, t)
	if c and c.valid and t then
		local s = c.screen
		if client.focus == c and s then
			set_last_focus(t, s, c)
		end
	end
end)

client.connect_signal("untagged", function(c, t)
	local rec = LAST_FOCUS[t]
	if not (rec and c) then
		return
	end
	if rec.last_any == c then
		rec.last_any = nil
	end
	for s, cl in pairs(rec.by_screen) do
		if cl == c then
			rec.by_screen[s] = nil
		end
	end
end)

client.connect_signal("unmanage", function(c)
	if not c then
		return
	end
	for t, rec in pairs(LAST_FOCUS) do
		if rec.last_any == c then
			rec.last_any = nil
		end
		for s, cl in pairs(rec.by_screen) do
			if cl == c then
				rec.by_screen[s] = nil
			end
		end
	end
end)

-- ===== Öffentliche API =================================================

function M.focus_master_current(s)
	s = s or awful.screen.focused()
	local t = s and s.selected_tag
	if not t then
		return
	end
	focus_last_of_tag_on_screen_or_fallback(t, s)
end

-- apply_layout_policy_fn wird von init.lua injiziert
function M.attach_policy_signals(apply_layout_policy_fn)
	tag.connect_signal("property::selected", function(t)
		if t.selected and t.screen then
			with_mouse_lock(function()
				if type(apply_layout_policy_fn) == "function" then
					apply_layout_policy_fn(t.screen)
				end
				focus_last_of_tag_on_screen_or_fallback(t, t.screen)
			end)
		end
	end)
end

return M
