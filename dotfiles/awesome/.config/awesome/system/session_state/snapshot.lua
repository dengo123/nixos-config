-- ~/.config/awesome/system/session_state/snapshot.lua
local awful = require("awful")

local M = {}

local runtime = {
	ctx = {},
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function ctx()
	return runtime.ctx or {}
end

local function screen_key(s)
	if s and type(s.outputs) == "table" then
		for name, active in pairs(s.outputs) do
			if active then
				return name
			end
		end
	end

	return s and ("screen:" .. tostring(s.index)) or "screen:?"
end

local function selected_tag(s)
	return s and s.selected_tag or nil
end

local function first_tag(c)
	return c and c.first_tag or nil
end

local function tag_name(t)
	return t and t.name or nil
end

local function current_layout_for_tag(t)
	if not t then
		return nil
	end

	return awful.layout.get(t.screen)
end

local function layout_name(layout_obj)
	local suit = awful.layout.suit

	local map = {
		[suit.tile] = "tile",
		[suit.tile.left] = "tile_left",
		[suit.tile.bottom] = "tile_bottom",
		[suit.tile.top] = "tile_top",
		[suit.fair] = "fair",
		[suit.fair.horizontal] = "fair_horizontal",
		[suit.spiral] = "spiral",
		[suit.spiral.dwindle] = "dwindle",
		[suit.max] = "max",
		[suit.max.fullscreen] = "max_fullscreen",
		[suit.magnifier] = "magnifier",
		[suit.corner.nw] = "corner_nw",
		[suit.corner.ne] = "corner_ne",
		[suit.corner.sw] = "corner_sw",
		[suit.corner.se] = "corner_se",
		[suit.floating] = "floating",
	}

	return map[layout_obj] or "tile"
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(args)
	runtime.ctx = (args and (args.ctx or args)) or {}
	return M
end

function M.current_state()
	local data = {
		version = 1,
		ts = os.time(),
		screens = {},
		clients = {},
	}

	for s in screen do
		local t = selected_tag(s)

		data.screens[screen_key(s)] = {
			selected_tag_name = tag_name(t),
			selected_tag_idx = t and t.index or nil,
			layout_name = t and layout_name(current_layout_for_tag(t)) or nil,
		}
	end

	for _, c in ipairs(client.get()) do
		local t = first_tag(c)

		table.insert(data.clients, {
			window = c.window,
			screen_key = c.screen and screen_key(c.screen) or nil,
			tag_name = tag_name(t),
			tag_idx = t and t.index or nil,
			minimized = c.minimized == true,
			floating = c.floating == true,
			fullscreen = c.fullscreen == true,
			maximized = c.maximized == true,
		})
	end

	return data
end

return M
