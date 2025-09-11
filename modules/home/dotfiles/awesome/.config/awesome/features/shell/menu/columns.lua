-- features/shell/menu/columns.lua
local wibox = require("wibox")
local P = require("features.shell.menu.primitives")

local Columns = {}

-- Baut die zwei Spalten und gibt eine API zurück:
--   local columns = Columns.build(left_items, right_items, theme)
--   root_widget = columns.widget
--   columns:set_left(items)
--   columns:set_right(items)
function Columns.build(left_items, right_items, t)
	t = t or {}

	local left_list = P.list_widget(left_items, t)
	local right_list = P.list_widget(right_items, t)

	local left_col = wibox.widget({
		left_list,
		forced_width = t.col_left_w or 320,
		widget = wibox.container.constraint,
	})

	local right_col = wibox.widget({
		right_list,
		forced_width = t.col_right_w or 220,
		widget = wibox.container.constraint,
	})

	local cols = wibox.widget({
		{
			left_col,
			right_col,
			spacing = t.col_spacing or 8,
			layout = wibox.layout.fixed.horizontal,
		},
		left = t.cols_pad_l or 6,
		right = t.cols_pad_r or 6,
		top = t.cols_pad_t or 6,
		bottom = t.cols_pad_b or 6,
		widget = wibox.container.margin,
	})

	local api = { widget = cols }

	function api:set_left(items)
		left_list.children = P.list_widget(items, t).children
	end

	function api:set_right(items)
		right_list.children = P.list_widget(items, t).children
	end

	return api
end

return Columns
