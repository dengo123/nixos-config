-- ~/.config/awesome/features/workspaces/tags/core.lua
local awful = require("awful")

local M = {}

-- Hooks (werden von init.lua gesetzt)
local HOOKS = {
	kill_clients_in_tag = function(_) end,
	apply_layout_policy = function(_) end,
}

function M.set_hooks(h)
	if type(h) ~= "table" then
		return
	end
	for k, v in pairs(h) do
		if HOOKS[k] and type(v) == "function" then
			HOOKS[k] = v
		end
	end
end

-- interne Helfer ------------------------------------------------------

local function renumber_tags(s)
	s = s or awful.screen.focused()
	for i, t in ipairs(s.tags or {}) do
		local want = tostring(i)
		if t.name ~= want then
			t.name = want
		end
	end
end

local function desired_layout_for(s)
	local g = s.geometry
	return (g.width >= g.height) and awful.layout.suit.tile or awful.layout.suit.tile.top
end

local function ensure_one_tag(s)
	s = s or awful.screen.focused()
	if #s.tags == 0 then
		awful.tag.add("1", {
			screen = s,
			layout = desired_layout_for(s),
			selected = true,
		})
	end
end

-- API -----------------------------------------------------------------

function M.ensure(s)
	ensure_one_tag(s)
end

function M.renumber(s)
	renumber_tags(s)
end

function M.add(s)
	s = s or awful.screen.focused()
	local name = tostring(#s.tags + 1)
	local t = awful.tag.add(name, {
		screen = s,
		layout = desired_layout_for(s),
		selected = true,
	})
	renumber_tags(s)
	return t
end

function M.delete_current(s)
	s = s or awful.screen.focused()
	local t = s and s.selected_tag
	if not t or #s.tags <= 1 then
		return
	end -- letzten Tag nie löschen

	local idx = t.index
	local fallback = s.tags[idx - 1] or s.tags[idx + 1]
	if fallback then
		fallback:view_only()
	end

	-- Clients entsprechend Policy behandeln (killen/enttaggen)
	HOOKS.kill_clients_in_tag(t)

	t:delete()
	ensure_one_tag(s)
	renumber_tags(s)
	HOOKS.apply_layout_policy(s)
end

return M
