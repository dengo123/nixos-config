-- ~/.config/awesome/features/workspaces/tags/focus_policy.lua
local awful = require("awful")
local gears = require("gears")

local M = {}

local function focus_master_of_tag(t)
	if not t then
		return
	end
	gears.timer.delayed_call(function()
		-- Kandidat 1: "Master" des Screens (nur wenn auf diesem Tag)
		local c = awful.client.getmaster(t.screen)
		if not (c and c.valid and c.first_tag == t) then
			-- Kandidat 2: erstes tiled-Fenster
			local tiled = awful.client.tiled(t)
			c = (tiled and tiled[1]) or nil
		end
		-- Fallback: irgendein Client dieses Tags
		if not (c and c.valid) then
			local clients = t:clients()
			c = clients and clients[1] or nil
		end

		if c and c.valid then
			if c.minimized then
				c.minimized = false
			end
			local focused = client.focus
			if not (focused and focused.valid and focused.first_tag == t) then
				c:emit_signal("request::activate", "tag_switch_master_focus", { raise = true })
			end
		end
	end)
end

-- öffentliche API -----------------------------------------------------

function M.focus_master_current(s)
	s = s or awful.screen.focused()
	if s and s.selected_tag then
		focus_master_of_tag(s.selected_tag)
	end
end

-- apply_layout_policy_fn wird von init.lua injiziert (entkoppelt Abhängigkeit)
function M.attach_policy_signals(apply_layout_policy_fn)
	tag.connect_signal("property::selected", function(t)
		if t.selected and t.screen then
			if type(apply_layout_policy_fn) == "function" then
				apply_layout_policy_fn(t.screen)
			end
			focus_master_of_tag(t)
		end
	end)
end

return M
