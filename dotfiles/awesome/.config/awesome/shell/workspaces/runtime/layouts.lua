-- ~/.config/awesome/shell/workspaces/runtime/layouts.lua
local awful = require("awful")

local M = {}

local concrete = {
	floating = awful.layout.suit.floating,

	tile = awful.layout.suit.tile,
	tile_left = awful.layout.suit.tile.left,
	tile_right = awful.layout.suit.tile.right,
	tile_bottom = awful.layout.suit.tile.bottom,
	tile_top = awful.layout.suit.tile.top,

	fair = awful.layout.suit.fair,
	fair_horizontal = awful.layout.suit.fair.horizontal,

	spiral = awful.layout.suit.spiral,
	spiral_dwindle = awful.layout.suit.spiral.dwindle,

	max = awful.layout.suit.max,
	max_fullscreen = awful.layout.suit.max.fullscreen,

	magnifier = awful.layout.suit.magnifier,

	corner_nw = awful.layout.suit.corner.nw,
	corner_ne = awful.layout.suit.corner.ne,
	corner_sw = awful.layout.suit.corner.sw,
	corner_se = awful.layout.suit.corner.se,
}

local aliases = {
	fair = true,
	max = true,
	tile_main = true,
	tile_cross = true,
}

local default_include = {
	"max",
	"fair",
	"tile_main",
	"tile_cross",
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function normalize_name(name)
	return tostring(name or ""):lower()
end

local function tags_cfg(cfg)
	return (cfg and cfg.tags) or {}
end

local function layouts_cfg(cfg)
	return tags_cfg(cfg).layouts or {}
end

local function layout_mode(cfg)
	local mode = tostring(layouts_cfg(cfg).mode or "tiling"):lower()
	if mode == "floating" then
		return "floating"
	end
	return "tiling"
end

local function resolve_alias_or_concrete(name)
	local key = normalize_name(name)

	if aliases[key] then
		return key
	end

	local layout = concrete[key]
	assert(layout ~= nil, "workspaces.layouts: layout ungueltig: " .. tostring(name))

	return layout
end

local function resolve_include_aliases(cfg)
	local include = layouts_cfg(cfg).include or default_include
	local out = {}

	for _, name in ipairs(include) do
		table.insert(out, resolve_alias_or_concrete(name))
	end

	return out
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.mode(cfg)
	return layout_mode(cfg)
end

function M.default_name(cfg)
	local lcfg = layouts_cfg(cfg)
	local name = tostring(lcfg.default or ((layout_mode(cfg) == "floating") and "floating" or "max")):lower()

	if layout_mode(cfg) == "floating" then
		return "floating"
	end

	return name
end

function M.use_max_fullscreen_for_portrait(cfg)
	return layouts_cfg(cfg).use_max_fullscreen_for_portrait == true
end

function M.resolve(name)
	local key = normalize_name(name)
	local layout = concrete[key]

	assert(layout ~= nil, "workspaces.layouts: layout ungueltig: " .. tostring(name))

	return layout
end

function M.resolve_include(cfg)
	if layout_mode(cfg) == "floating" then
		return { concrete.floating }
	end

	local out = {}

	for _, entry in ipairs(resolve_include_aliases(cfg)) do
		if type(entry) ~= "string" or entry ~= "floating" then
			table.insert(out, entry)
		end
	end

	return out
end

function M.apply(cfg)
	awful.layout.layouts = M.resolve_include(cfg)
end

function M.is_alias(name)
	return aliases[normalize_name(name)] == true
end

function M.is_concrete_name(name)
	return concrete[normalize_name(name)] ~= nil
end

return M
