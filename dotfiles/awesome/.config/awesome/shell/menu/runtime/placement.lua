-- ~/.config/awesome/shell/menu/runtime/placement.lua
local P = {}

local runtime = {
	layout = nil,
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function layout_mod()
	return runtime.layout
end

local function get_layout()
	local Layout = layout_mod()
	local out = (Layout and type(Layout.get) == "function" and Layout.get()) or {}

	if type(out) ~= "table" then
		return {}
	end

	return out
end

local function bar_object(s)
	return s and (s.mywibar or s.mywibox or s.wibar) or nil
end

local function bar_geometry(s)
	local wibar = bar_object(s)
	if wibar and wibar.valid and wibar._menu_anchor == true then
		return wibar:geometry()
	end

	return nil
end

local function screen_geometry(s)
	if s and s.geometry then
		return s.geometry
	end

	return {
		x = 0,
		y = 0,
		width = 0,
		height = 0,
	}
end

local function wibar_position(s)
	local wibar = bar_object(s)
	if wibar and wibar.valid and wibar._menu_anchor == true and wibar.position then
		return wibar.position
	end

	return "bottom"
end

local function layout_width()
	local layout = get_layout()
	return tonumber(layout.width) or 220
end

local function screen_workarea(s)
	if s and s.workarea then
		return s.workarea
	end

	return screen_geometry(s)
end

local function has_bar(s)
	local wibar = bar_object(s)

	if not (wibar and wibar.valid) then
		return false
	end

	return wibar._menu_anchor == true
end

local function total_height(item_count)
	local Layout = layout_mod()
	if Layout and type(Layout.total_height) == "function" then
		return Layout.total_height(item_count)
	end

	return 0
end

-- =========================================================================
-- Public API
-- =========================================================================

function P.init(opts)
	opts = opts or {}
	runtime.layout = opts.layout or runtime.layout
	return P
end

function P.y_over_bar(s, menu_height)
	local layout = get_layout()
	local gap = tonumber(layout.gap) or 4

	local position = wibar_position(s)
	local bar = bar_geometry(s)

	if bar then
		if position == "bottom" then
			return bar.y - menu_height - gap
		end

		return bar.y + bar.height + gap
	end

	local sg = screen_geometry(s)
	local fallback_bar_height = 28

	return sg.y + math.max(0, sg.height - menu_height - (fallback_bar_height + gap))
end

function P.x_left_from_anchor(_s, x_left)
	local layout = get_layout()
	local x_padding = tonumber(layout.x_padding) or 0
	local x_offset = tonumber(layout.x_offset) or 0

	return (x_left or 0) + x_padding + x_offset
end

function P.x_left_on_bar(s)
	local layout = get_layout()
	local x_padding = tonumber(layout.x_padding) or 0
	local x_offset = tonumber(layout.x_offset) or 0

	local bar = bar_geometry(s)
	local base_x = bar and bar.x or screen_geometry(s).x

	return base_x + x_padding + x_offset
end

function P.coords_for_tabs(s, anchor_left_x, item_count)
	local x = P.x_left_from_anchor(s, anchor_left_x)
	local y = P.y_over_bar(s, total_height(item_count))

	return x, y
end

function P.coords_for_start(s, item_count)
	local x = P.x_left_on_bar(s)
	local y = P.y_over_bar(s, total_height(item_count))

	return x, y
end

function P.coords_centered(s, item_count)
	local wa = screen_workarea(s)

	local width = layout_width()
	local menu_height = total_height(item_count)

	local x = wa.x + math.floor((wa.width - width) / 2)
	local y = wa.y + math.floor((wa.height - menu_height) / 2)

	if x < wa.x then
		x = wa.x
	end

	if y < wa.y then
		y = wa.y
	end

	return x, y
end

function P.coords_for_keybind(s, item_count)
	if has_bar(s) then
		return P.coords_for_start(s, item_count)
	end

	return P.coords_centered(s, item_count)
end

return P
