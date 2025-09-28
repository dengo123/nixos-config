-- ~/.config/awesome/features/shell/menu/parts/columns.lua
local wibox = require("wibox")
local P = require("features.shell.menu.widgets")
local Theme = require("features.shell.menu.lib.theme")

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
	t = Theme.with_defaults(t)
	opts = opts or {}

	-- Spaltenspezifische Paletten + Höhen
	local left_colors = Theme.row_colors(t, "left")
	local right_colors = Theme.row_colors(t, "right")
	local left_row_h = pick(t.left_row_h, t.row_h)
	local right_row_h = pick(t.right_row_h, t.row_h)

	-- eigene Opts pro Liste (nicht teilen!)
	local left_opts = shallow_copy(opts)
	left_opts.colors = left_colors
	left_opts.row_h = left_row_h

	local right_opts = shallow_copy(opts)
	right_opts.colors = right_colors
	right_opts.row_h = right_row_h

	-- Views bauen (Rows lesen ausschließlich opts.colors/row_h)
	local left_focus, right_focus
	local left_view, lf = P.list_widget(left_items or {}, t, left_opts)
	left_focus = lf
	local right_view, rf = P.list_widget(right_items or {}, t, right_opts)
	right_focus = rf

	-- Spalten-Container (Hintergrund = Spaltenfarbe)
	local left_col = wibox.widget({
		{ left_view, margins = 0, widget = wibox.container.margin },
		forced_width = pick(t.col_left_w, 250),
		bg = left_colors.bg,
		fg = left_colors.fg,
		shape = t.col_shape,
		widget = wibox.container.background,
	})

	local right_col = wibox.widget({
		{ right_view, margins = 0, widget = wibox.container.margin },
		forced_width = pick(t.col_right_w, 230),
		bg = right_colors.bg,
		fg = right_colors.fg,
		shape = t.col_shape,
		widget = wibox.container.background,
	})

	-- Spalten nebeneinander
	local cols_inner = wibox.widget({
		left_col,
		right_col,
		spacing = pick(t.col_spacing, 1),
		layout = wibox.layout.fixed.horizontal,
	})

	-- Außencontainer (Columns-Hintergrund)
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
		-- Farben/Höhen beibehalten
		local v, foc = P.list_widget(items or {}, t, left_opts)
		left_view.children = v.children
		left_focus = foc or {}
	end

	function api:set_right(items)
		local v, foc = P.list_widget(items or {}, t, right_opts)
		right_view.children = v.children
		right_focus = foc or {}
	end

	function api:get_focus_items()
		return { left = left_focus or {}, right = right_focus or {} }
	end

	return api
end

return Columns
