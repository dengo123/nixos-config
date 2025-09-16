-- ~/.config/awesome/features/shell/menu/components/columns.lua
local wibox = require("wibox")
local gears = require("gears")
local P = require("features.shell.menu.parts.widgets")

local Columns = {}

local DEFAULTS = {
	col_left_w = 250,
	col_right_w = 230,
	col_spacing = 1,
	cols_pad_l = 2,
	cols_pad_r = 2,
	cols_pad_t = 2,
	cols_pad_b = 2,

	-- Spaltenfarben
	left_bg = "#FFFFFF", -- weiß
	left_fg = "#000000",
	right_bg = "#D2E5FA", -- hellblau
	right_fg = "#000000",

	-- Rahmenfarbe (sichtbar durch padding)
	border_bg = "#3A6EA5", -- Luna-Blau

	shape = gears.shape.rectangle,
}

local function merge(base, override)
	local out = {}
	for k, v in pairs(base) do
		out[k] = v
	end
	for k, v in pairs(override or {}) do
		out[k] = v
	end
	return out
end

function Columns.build(left_items, right_items, t)
	t = merge(DEFAULTS, t or {})

	-- eigene Theme-Overrides pro Spalte
	local left_t = merge(t, {
		row_bg = t.left_bg, -- weiße Buttons
		row_fg = t.left_fg,
		row_h = 48, -- 48px links
		list_spacing = 0, -- KEIN sichtbarer Reihenabstand
		-- optional, falls du „stramm“ willst:
		-- row_pad_t = 0, row_pad_b = 0
	})

	local right_t = merge(t, {
		row_bg = t.right_bg, -- hellblaue Buttons
		row_fg = t.right_fg,
		row_h = 40, -- 40px rechts
		list_spacing = 0, -- KEIN sichtbarer Reihenabstand
		-- Hover wird automatisch leicht dunkler (kommt aus primitives)
		-- row_pad_t = 0, row_pad_b = 0
	})

	local left_list = P.list_widget(left_items, left_t)
	local right_list = P.list_widget(right_items, right_t)

	-- linke Spalte
	local left_col = wibox.widget({
		{ left_list, margins = 0, widget = wibox.container.margin },
		forced_width = t.col_left_w,
		bg = t.left_bg,
		fg = t.left_fg,
		shape = t.shape,
		widget = wibox.container.background,
	})

	-- rechte Spalte
	local right_col = wibox.widget({
		{ right_list, margins = 0, widget = wibox.container.margin },
		forced_width = t.col_right_w,
		bg = t.right_bg,
		fg = t.right_fg,
		shape = t.shape,
		widget = wibox.container.background,
	})

	-- beide Spalten nebeneinander
	local cols_inner = wibox.widget({
		left_col,
		right_col,
		spacing = t.col_spacing, -- Abstand ZWISCHEN den Spalten (ok)
		layout = wibox.layout.fixed.horizontal,
	})

	-- Äußerer Container mit Luna-Blau
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

	local api = { widget = cols }

	function api:set_left(items)
		left_list.children = P.list_widget(items, left_t).children
	end

	function api:set_right(items)
		right_list.children = P.list_widget(items, right_t).children
	end

	return api
end

return Columns
