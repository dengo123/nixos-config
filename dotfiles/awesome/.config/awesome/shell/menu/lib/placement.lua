local P = {}

local runtime_api = {}

-- =========================================================================
-- Helpers
-- =========================================================================

local function layout_api()
	return runtime_api.layout
end

local function get_layout()
	local Layout = layout_api()
	local out = Layout and Layout.get and Layout.get() or {}
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

-- =========================================================================
-- Public API
-- =========================================================================

function P.init(args)
	args = args or {}
	runtime_api = args.api or args or {}
end

function P.y_over_bar(s, total_height)
	local layout = get_layout()
	local gap = tonumber(layout.gap) or 4

	local position = wibar_position(s)
	local bar = bar_geometry(s)

	if bar then
		if position == "bottom" then
			return bar.y - total_height - gap
		end

		return bar.y + bar.height + gap
	end

	local screen = screen_geometry(s)
	local fallback_bar_height = 28

	return screen.y + math.max(0, screen.height - total_height - (fallback_bar_height + gap))
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
	local Layout = layout_api()
	local total_height = (Layout and Layout.total_height and Layout.total_height(item_count)) or 0
	local x = P.x_left_from_anchor(s, anchor_left_x)
	local y = P.y_over_bar(s, total_height)

	return x, y
end

function P.coords_for_start(s, item_count)
	local Layout = layout_api()
	local total_height = (Layout and Layout.total_height and Layout.total_height(item_count)) or 0
	local x = P.x_left_on_bar(s)
	local y = P.y_over_bar(s, total_height)

	return x, y
end

function P.coords_centered(s, item_count)
	local Layout = layout_api()
	local wa = screen_workarea(s)

	local width = layout_width()
	local total_height = (Layout and Layout.total_height and Layout.total_height(item_count)) or 0

	local x = wa.x + math.floor((wa.width - width) / 2)
	local y = wa.y + math.floor((wa.height - total_height) / 2)

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
