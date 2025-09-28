-- ~/.config/awesome/features/shell/menu/layouts/columns.lua
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

-- opts unterstützt u.a.:
--   left_w?, right_w?, spacing?, pad_l?, pad_r?, pad_t?, pad_b?
--   left_bg?, right_bg?
--   left_opts?, right_opts?  -- gehen 1:1 an P.list_widget (inkl. colors, row_h)
function Columns.build(left_items, right_items, t, opts)
	t = t or {}
	opts = opts or {}

	-- pro Seite eigene opts weiterreichen
	local left_opts = shallow_copy(opts.left_opts or {})
	local right_opts = shallow_copy(opts.right_opts or {})

	local left_view, left_focus = P.list_widget(left_items or {}, t, left_opts)
	local right_view, right_focus = P.list_widget(right_items or {}, t, right_opts)

	-- Spaltenhintergrund (durchgängig) – komplett theme-agnostisch:
	local left_col = wibox.widget({
		{ left_view, margins = 0, widget = wibox.container.margin },
		forced_width = tonumber(opts.left_w) or nil,
		bg = opts.left_bg, -- <— NEU: optionale Farbe
		widget = wibox.container.background,
	})

	local right_col = wibox.widget({
		{ right_view, margins = 0, widget = wibox.container.margin },
		forced_width = tonumber(opts.right_w) or nil,
		bg = opts.right_bg, -- <— NEU: optionale Farbe
		widget = wibox.container.background,
	})

	local cols_inner = wibox.widget({
		left_col,
		right_col,
		spacing = tonumber(opts.spacing) or 0,
		layout = wibox.layout.fixed.horizontal,
	})

	local cols = wibox.widget({
		{
			cols_inner,
			left = tonumber(opts.pad_l) or 0,
			right = tonumber(opts.pad_r) or 0,
			top = tonumber(opts.pad_t) or 0,
			bottom = tonumber(opts.pad_b) or 0,
			widget = wibox.container.margin,
		},
		widget = wibox.container.background,
	})

	local api = { widget = cols }

	function api:set_left(items)
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
