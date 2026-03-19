-- ~/.config/awesome/shell/menu/lib/placement.lua
local P = {}

-- =========================================================================
-- Helpers
-- =========================================================================

local function get_menu_theme(ui)
	local theme = ui and ui.theme and ui.theme.menu

	assert(theme, "shell.menu: ui.theme.menu fehlt")

	return theme
end

local function get_theme_props(ui)
	local theme = get_menu_theme(ui)

	if type(theme.props) == "function" then
		local props = theme.props()
		assert(type(props) == "table", "shell.menu: ui.theme.menu.props() lieferte kein table")
		return props
	end

	if type(theme.get) == "function" then
		local menu = theme.get()
		assert(type(menu) == "table", "shell.menu: ui.theme.menu.get() lieferte kein table")

		return {
			item_height = menu.height,
			width = menu.width,
			gap = 4,
			x_padding = 0,
			align = "left",
			x_offset = 0,
		}
	end

	error("shell.menu: ui.theme.menu.props() fehlt")
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

function P.y_over_bar(ui, s, total_height)
	local props = get_theme_props(ui)
	local gap = tonumber(props.gap) or 4

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

function P.x_left_from_anchor(ui, _s, x_left)
	local props = get_theme_props(ui)
	local x_padding = tonumber(props.x_padding) or 0
	local x_offset = tonumber(props.x_offset) or 0

	return (x_left or 0) + x_padding + x_offset
end

function P.x_left_on_bar(ui, s)
	local props = get_theme_props(ui)
	local x_padding = tonumber(props.x_padding) or 0
	local x_offset = tonumber(props.x_offset) or 0

	local bar = bar_geometry(s)
	local base_x = bar and bar.x or screen_geometry(s).x

	return base_x + x_padding + x_offset
end

function P.item_height(ui)
	local props = get_theme_props(ui)
	return assert(tonumber(props.item_height), "shell.menu: theme.props().item_height ungültig")
end

function P.total_height(ui, item_count)
	local height = P.item_height(ui)
	return height * (item_count or 0)
end

function P.coords_for_tabs(ui, s, anchor_left_x, item_count)
	local total_height = P.total_height(ui, item_count)
	local x = P.x_left_from_anchor(ui, s, anchor_left_x)
	local y = P.y_over_bar(ui, s, total_height)

	return x, y
end

function P.coords_for_start(ui, s, item_count)
	local total_height = P.total_height(ui, item_count)
	local x = P.x_left_on_bar(ui, s)
	local y = P.y_over_bar(ui, s, total_height)

	return x, y
end

return P
