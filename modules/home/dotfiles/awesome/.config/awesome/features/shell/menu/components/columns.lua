-- ~/.config/awesome/features/shell/menu/components/columns.lua
local wibox = require("wibox")
local gears = require("gears")
local P = require("features.shell.menu.parts.widgets")

local Columns = {}

local DEFAULTS = {
	-- Geometrie
	col_left_w = 250,
	col_right_w = 230,
	col_spacing = 1,

	-- Außenabstände (Rahmen sichtbar machen)
	cols_pad_l = 2,
	cols_pad_r = 2,
	cols_pad_t = 2,
	cols_pad_b = 2,

	-- Spaltenfarben
	left_bg = "#FFFFFF",
	left_fg = "#000000",
	right_bg = "#D2E5FA",
	right_fg = "#000000",

	-- Rahmen-/Hintergrund der gesamten Zwei-Spalten-Ansicht
	border_bg = "#3A6EA5", -- Luna-Blau

	-- Spaltenform
	col_shape = gears.shape.rectangle,

	-- Row-Höhen (an widgets.lua durchgereicht)
	row_h = 48, -- globaler Default
	left_row_h = nil, -- optional spezieller Override
	right_row_h = 40, -- z.B. rechte Spalte etwas kompakter
	list_spacing = 0, -- kein sichtbarer Abstand zwischen Rows
}

local function merge(a, b)
	local out = {}
	for k, v in pairs(a or {}) do
		out[k] = v
	end
	for k, v in pairs(b or {}) do
		out[k] = v
	end
	return out
end

function Columns.build(left_items, right_items, t)
	t = merge(DEFAULTS, t or {})

	-- Spalten-spezifische Theme-Overlays für die Rows:
	-- (Hier KEINE Button-/Hover-/Click-Logik mehr; das macht widgets.lua)
	local left_t = merge(t, {
		row_bg = t.left_bg,
		row_fg = t.left_fg,
		row_h = t.left_row_h or t.row_h,
		list_spacing = t.list_spacing,
	})

	local right_t = merge(t, {
		row_bg = t.right_bg,
		row_fg = t.right_fg,
		row_h = t.right_row_h or t.row_h,
		list_spacing = t.list_spacing,
	})

	-- Rows kommen vollständig aus widgets.lua (P.list_widget / P.row_widget)
	local left_list = P.list_widget(left_items or {}, left_t)
	local right_list = P.list_widget(right_items or {}, right_t)

	-- Linke Spalte
	local left_col = wibox.widget({
		{ left_list, margins = 0, widget = wibox.container.margin },
		forced_width = t.col_left_w,
		bg = t.left_bg,
		fg = t.left_fg,
		shape = t.col_shape,
		widget = wibox.container.background,
	})

	-- Rechte Spalte
	local right_col = wibox.widget({
		{ right_list, margins = 0, widget = wibox.container.margin },
		forced_width = t.col_right_w,
		bg = t.right_bg,
		fg = t.right_fg,
		shape = t.col_shape,
		widget = wibox.container.background,
	})

	-- Beide Spalten nebeneinander
	local cols_inner = wibox.widget({
		left_col,
		right_col,
		spacing = t.col_spacing,
		layout = wibox.layout.fixed.horizontal,
	})

	-- Äußerer Container (Rahmenfläche)
	local cols = wibox.widget({
		{
			cols_inner,
			left = t.cols_pad_l,
			right = t.cols_pad_r,
			top = t.cols_pad_t,
			bottom = t.cols_pad_b,
			widget = wibox.container.margin,
		},
		bg = t.border_bg,
		widget = wibox.container.background,
	})

	-- kleines API zum Live-Aktualisieren
	local api = { widget = cols }
	function api:set_left(items)
		left_list.children = P.list_widget(items or {}, left_t).children
	end

	function api:set_right(items)
		right_list.children = P.list_widget(items or {}, right_t).children
	end

	return api
end

return Columns
