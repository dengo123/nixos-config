-- ~/.config/awesome/features/shell/menu/parts/columns.lua
local wibox = require("wibox")
local P = require("features.shell.menu.widgets")

local Columns = {}

local function shallow_copy(tbl)
	local out = {}
	for k, v in pairs(tbl or {}) do
		out[k] = v
	end
	return out
end
local function pick(...)
	for i = 1, select("#", ...) do
		local v = select(i, ...)
		if v ~= nil then
			return v
		end
	end
end

function Columns.build(left_items, right_items, t, opts)
	local left_t = shallow_copy(t)
	left_t.row_bg = pick(t.left_bg, t.row_bg)
	left_t.row_fg = pick(t.left_fg, t.row_fg)
	left_t.row_h = pick(t.left_row_h, t.row_h)
	left_t.list_spacing = pick(t.left_list_spacing, t.list_spacing)

	local right_t = shallow_copy(t)
	right_t.row_bg = pick(t.right_bg, t.row_bg)
	right_t.row_fg = pick(t.right_fg, t.row_fg)
	right_t.row_h = pick(t.right_row_h, t.row_h)
	right_t.list_spacing = pick(t.right_list_spacing, t.list_spacing)

	-- WICHTIG: zweite Rückgabe (Fokus-Items) entgegennehmen
	local left_view, left_focus = P.list_widget(left_items or {}, left_t, opts)
	local right_view, right_focus = P.list_widget(right_items or {}, right_t, opts)

	local left_col = wibox.widget({
		{ left_view, margins = 0, widget = wibox.container.margin },
		forced_width = pick(t.col_left_w, 250),
		bg = left_t.row_bg,
		fg = left_t.row_fg,
		shape = t.col_shape,
		widget = wibox.container.background,
	})

	local right_col = wibox.widget({
		{ right_view, margins = 0, widget = wibox.container.margin },
		forced_width = pick(t.col_right_w, 230),
		bg = right_t.row_bg,
		fg = right_t.row_fg,
		shape = t.col_shape,
		widget = wibox.container.background,
	})

	local cols_inner = wibox.widget({
		left_col,
		right_col,
		spacing = pick(t.col_spacing, 1),
		layout = wibox.layout.fixed.horizontal,
	})

	local cols = wibox.widget({
		{
			cols_inner,
			left = pick(t.cols_pad_l, 2),
			right = pick(t.cols_pad_r, 2),
			top = pick(t.cols_pad_t, 2),
			bottom = pick(t.cols_pad_b, 2),
			widget = wibox.container.margin,
		},
		bg = pick(t.dialog_bg, "#235CDB"),
		widget = wibox.container.background,
	})

	local api = { widget = cols }

	function api:set_left(items)
		local v, foc = P.list_widget(items or {}, left_t, opts)
		left_view.children = v.children
		left_focus = foc or {}
	end

	function api:set_right(items)
		local v, foc = P.list_widget(items or {}, right_t, opts)
		right_view.children = v.children
		right_focus = foc or {}
	end

	-- NEU: Fokus-Items nach außen geben
	function api:get_focus_items()
		return { left = left_focus or {}, right = right_focus or {} }
	end

	return api
end

return Columns
