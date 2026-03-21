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

local function bar_geometry(s)
	local wibar = s and (s.mywibar or s.mywibox or s.wibar)
	if wibar and wibar.valid then
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
	local wibar = s and (s.mywibar or s.mywibox or s.wibar)
	if wibar and wibar.position then
		return wibar.position
	end

	return "bottom"
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

return P
