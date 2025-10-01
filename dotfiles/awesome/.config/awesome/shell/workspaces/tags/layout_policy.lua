-- ~/.config/awesome/features/workspaces/tags/layout_policy.lua
local awful = require("awful")
local layouts = require("shell.workspaces.layouts") -- Pfad ggf. anpassen

local M = {}

local function in_allowed(cur, allowed)
	for _, L in ipairs(allowed) do
		if L == cur then
			return true
		end
	end
	return false
end

function M.apply_layout_policy(s)
	s = s or awful.screen.focused()
	if not s or not s.selected_tag then
		return
	end
	local t = s.selected_tag
	local allowed = layouts.allowed_for(s)
	-- (Optional) den Layout-Chooser/Indicator informieren:
	-- t.layouts = allowed   -- funktioniert mit manchen Widgets (Layoutlist)

	if not in_allowed(t.layout, allowed) then
		-- außerhalb der erlaubten Menge → auf Standard (max = allowed[1]) setzen
		t.layout = allowed[1]
	end
end

function M.apply_layout_policy_all(s)
	s = s or awful.screen.focused()
	if not s then
		return
	end
	local allowed = layouts.allowed_for(s)
	for _, t in ipairs(s.tags or {}) do
		-- t.layouts = allowed  -- optional
		if not in_allowed(t.layout, allowed) then
			t.layout = allowed[1]
		end
	end
end

function M.on_screen_rotation()
	screen.connect_signal("property::geometry", function(s)
		M.apply_layout_policy_all(s)
	end)
end

return M
